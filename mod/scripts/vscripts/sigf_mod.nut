// Doom Takeover: every fighter is swapped for a 3D Doom body (marines vs imps and cacodemons).
::DoomBody <- {}          // player entindex -> { ent, kind, painUntil, ph }
::DoomCorpses <- []
::DoomHudUntil <- 0.0
::DoomKills <- 0

const DOOM_EF_NODRAW = 32

foreach (m in ["imp", "mar", "cac"]) PrecacheModel("models/sigf/" + m + ".mdl")
PrecacheModel("models/passtime/skull/passtime_skull.mdl")
PrecacheSound("sigf/imp_roar.wav")
PrecacheSound("sigf/shotgun.wav")
PrecacheSound("sigf/splat.wav")

::DoomKind <- function(p) {
	if (p.GetTeam() == 2) return "mar"
	return (p.entindex() % 3 == 0) ? "cac" : "imp"
}

::DoomHide <- function(p) {
	NetProps.SetPropInt(p, "m_fEffects", NetProps.GetPropInt(p, "m_fEffects") | DOOM_EF_NODRAW)
	for (local c = p.FirstMoveChild(); c != null; c = c.NextMovePeer())
		NetProps.SetPropInt(c, "m_fEffects", NetProps.GetPropInt(c, "m_fEffects") | DOOM_EF_NODRAW)
}

// Runs every frame on each Doom body: follows its player and animates walk, lunge and pain.
::DoomFollow <- function() {
	local b = self.GetScriptScope().b
	local p = b.owner
	if (p == null || !p.IsValid() || !p.IsAlive()) return 0.5
	local now = Time()
	local speed = p.GetAbsVelocity().Length()
	local ph = now * 11.0 + p.entindex()
	local lift = b.kind == "cac" ? 28.0 + sin(now * 3.0 + p.entindex()) * 6.0 : 0.0
	local roll = 0.0
	if (speed > 40 && b.kind != "cac") { lift += fabs(sin(ph)) * 4.0; roll = sin(ph) * 7.0 }
	if (now < b.painUntil) roll = 14.0
	local lean = b.kind == "cac" ? sin(now * 2.0) * 8.0 : 0.0
	local fwd = p.GetAbsAngles().Forward()
	local off = Vector(0, 0, lift)
	if (now < b.atkUntil) { lean += 16.0; off = off + fwd * 14.0 }
	self.SetAbsOrigin(p.GetOrigin() + off)
	self.SetAbsAngles(QAngle(lean, p.GetAbsAngles().y, roll))
	return 0.0
}

::DoomDress <- function(p) {
	local id = p.entindex()
	if (id in ::DoomBody && ::DoomBody[id].ent.IsValid()) { ::DoomHide(p); return }
	local kind = ::DoomKind(p)
	local e = SpawnEntityFromTable("prop_dynamic", {
		model = "models/sigf/" + kind + ".mdl", origin = p.GetOrigin(), angles = p.GetAbsAngles().ToKVString(), solid = 0, DefaultAnim = "idle",
	})
	e.SetModelScale(kind == "cac" ? 1.4 : (kind == "mar" ? 1.35 : 1.6), 0.0)
	local b = { ent = e, owner = p, kind = kind, painUntil = 0.0, hurt = false, atkUntil = 0.0 }
	::DoomBody[id] <- b
	e.ValidateScriptScope()
	e.GetScriptScope().b <- b
	e.GetScriptScope().Think <- ::DoomFollow
	AddThinkToEnt(e, "Think")
	::DoomHide(p)
}

::DoomTick <- function() {
	local now = Time()
	foreach (p in SigfPlayers()) {
		::DoomDress(p)
		local b = ::DoomBody[p.entindex()]
		local e = b.ent
		local hurt = now < b.painUntil
		if (hurt != b.hurt) {
			b.hurt = hurt
			e.AcceptInput("Color", hurt ? "255 70 70" : "255 255 255", null, null)
		}
	}
	// real ragdolls are replaced by the Doom bodies falling over
	local r = null
	while (r = Entities.FindByClassname(r, "tf_ragdoll")) r.Kill()
	while (r = Entities.FindByClassname(r, "tf_dropped_weapon")) r.Kill()
	::DoomCorpses = ::DoomCorpses.filter(function(i, c) {
		if (!c.ent.IsValid()) return false
		local t = now - c.t0
		if (t > 3.5) { c.ent.Kill(); return false }
		local k = t < 0.5 ? t / 0.5 : 1.0
		c.ent.SetAbsOrigin(c.pos + Vector(0, 0, c.kind == "cac" ? 28.0 * (1 - k) : 0))
		c.ent.SetAbsAngles(QAngle(0, c.yaw, c.kind == "cac" ? 0 : 88.0 * k))
		return true
	})
	local host = GetListenServerHost()
	if (host != null) host.SetScriptOverlayMaterial(now < ::DoomHudUntil ? "sigf/doom_o2" : "sigf/doom_o1")
}

// Caption low on the screen, clear of the game's hint panel.
::DoomCaption <- function(text, sec = 3.0) {
	local t = SpawnEntityFromTable("game_text", {
		message = text, x = -1, y = 0.64, effect = 0, color = "255 60 30", color2 = "255 255 255",
		fadein = 0.05, fadeout = 0.4, holdtime = sec, fxtime = 0, channel = 2, spawnflags = 1,
	})
	EntFireByHandle(t, "Display", "", 0, null, null)
	EntFireByHandle(t, "Kill", "", sec + 1.0, null, null)
}
::DoomShouts <- ["RIP AND TEAR!", "DEMON DOWN!", "BFG TIME!", "ONLY ONE THING LEFT TO DO!", "HELL YEAH!", "KNEE DEEP IN THE DEAD"]

::DoomEvents <- {
	OnGameEvent_player_hurt = function(params) {
		local v = GetPlayerFromUserID(params.userid)
		if (v == null) return
		local id = v.entindex()
		if (id in ::DoomBody) ::DoomBody[id].painUntil = Time() + 0.2
		DispatchParticleEffect("blood_impact_red_01", v.GetOrigin() + Vector(0, 0, 45), Vector(0, 0, 0))
		local a = GetPlayerFromUserID(params.attacker)
		if (a != null && a != v) {
			if (a.entindex() in ::DoomBody) {
				::DoomBody[a.entindex()].atkUntil = Time() + 0.3
				if (::DoomBody[a.entindex()].kind == "mar") DispatchParticleEffect("muzzle_shotgun", a.GetOrigin() + a.GetAbsAngles().Forward() * 40 + Vector(0, 0, 55), Vector(0, 0, 0))
			}
			if (RandomInt(0, 2) == 0) SigfSound("sigf/shotgun.wav", a.GetOrigin(), 85)
			local d = v.GetOrigin() - a.GetOrigin()
			d.z = 0
			d.Norm()
			v.ApplyAbsVelocityImpulse(d * 140 + Vector(0, 0, 60))
		}
	}
	OnGameEvent_player_death = function(params) {
		local v = GetPlayerFromUserID(params.userid)
		if (v == null) return
		local pos = v.GetOrigin()
		local id = v.entindex()
		if (id in ::DoomBody) {
			local b = ::DoomBody[id]
			if (b.ent.IsValid()) {
				AddThinkToEnt(b.ent, null)
				b.ent.AcceptInput("Color", "255 255 255", null, null)
				::DoomCorpses.append({ ent = b.ent, pos = pos, yaw = v.GetAbsAngles().y, kind = b.kind, t0 = Time() })
			}
			delete ::DoomBody[id]
		}
		local fx = pos + Vector(0, 0, 40)
		DispatchParticleEffect("blood_impact_red_01", fx, Vector(0, 0, 0))
		DispatchParticleEffect("ExplosionCore_MidAir", fx, Vector(0, 0, 0))
		for (local i = 0; i < 3; i++) {
			local s = SigfProp("models/passtime/skull/passtime_skull.mdl", fx, 1.2, 5.0)
			s.SetPhysVelocity(Vector(RandomInt(-220, 220), RandomInt(-220, 220), RandomInt(250, 450)))
		}
		SigfSound("sigf/splat.wav", pos, 90)
		SigfSound("sigf/shotgun.wav", pos, 95)
		if (RandomInt(0, 1) == 0) SigfSound("sigf/imp_roar.wav", pos, 90)
		::DoomHudUntil = Time() + 0.3
		::DoomKills++
		if (::DoomKills % 2 == 1) ::DoomCaption(::DoomShouts[RandomInt(0, ::DoomShouts.len() - 1)], 2.0)
	}
	OnGameEvent_teamplay_round_start = function(params) {
		::DoomBody = {}
		::DoomCorpses = []
	}
}
__CollectGameEventCallbacks(::DoomEvents)

SigfEvery(0.1, function() { ::DoomTick() })
SigfAfter(2.0, function() { ::DoomCaption("DOOM TAKEOVER", 5.0) })
