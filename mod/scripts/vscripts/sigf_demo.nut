SigfShowcase("soldier")

// Face the most open direction, then line up a small Doom battle in front of the player.
::OpenYaw <- function() {
	local host = SigfHost()
	local eye = host.EyePosition()
	local best = -1.0
	local bestYaw = 0.0
	for (local y = 0; y < 360; y += 10) {
		local m = 1.0
		foreach (o in [-20, -10, 0, 10, 20]) {
			foreach (z in [0, -30]) {
				local r = (y + o) * 0.0174533
				local f = TraceLine(eye + Vector(0, 0, z), eye + Vector(cos(r), sin(r), 0) * 800 + Vector(0, 0, z), host)
				if (f < m) m = f
			}
		}
		if (m > best) { best = m; bestYaw = y.tofloat() }
	}
	return bestYaw
}

// Put a bot in front of the player, never behind a wall: shorten the distance if the view is blocked.
::Place <- function(p, fwd, side, d, lat) {
	local host = SigfHost()
	local eye = host.EyePosition()
	local tgt = eye + fwd * d + side * lat
	local f = TraceLine(eye, tgt, host)
	if (f < 1.0) { d = d * f - 50; if (d < 120) d = 120 }
	SigfTeleport(p, host.GetOrigin() + fwd * d + side * lat * (d / 300.0) + Vector(0, 0, 12), Vector(0, 0, 0))
}
::StageYaw <- -999.0
::Aiming <- false
::Arena <- function(nBlu, nRed) {
	local host = SigfHost()
	local cp = Entities.FindByClassname(null, "team_control_point")
	if (cp != null) host.SetAbsOrigin(cp.GetOrigin() + Vector(0, 0, 40))
	NetProps.SetPropInt(host, "m_nForceTauntCam", 1)
	local yaw = ::OpenYaw()
	::StageYaw <- yaw
	host.SnapEyeAngles(QAngle(5, yaw, 0))
	SendToConsole("thirdperson; cam_idealdist 190; cam_idealdistup 40")
	local r = yaw * 0.0174533
	local fwd = Vector(cos(r), sin(r), 0)
	local side = Vector(-sin(r), cos(r), 0)
	local blu = []
	local red = []
	foreach (p in SigfPlayers()) {
		if (p == host) continue
		if (p.GetTeam() == 3) { blu.append(p) } else { red.append(p) }
	}
	local org = host.GetOrigin()
	for (local i = 0; i < nBlu && i < blu.len(); i++)
		::Place(blu[i], fwd, side, 230 + i * 30, (i - nBlu / 2) * 70)
	for (local i = 0; i < nRed && i < red.len(); i++)
		::Place(red[i], fwd, side, 400 + i * 30, (i - nRed / 2) * 70)
	foreach (p in blu) { p.AddCustomAttribute("move speed bonus", 0.3, 14.0); p.AddCustomAttribute("max health additive bonus", 1200, 40.0); p.SetHealth(p.GetMaxHealth() + 1200); p.StunPlayer(25.0, 0.85, 1, null) }
	foreach (p in red) { p.AddCustomAttribute("move speed bonus", 0.3, 14.0); p.AddCustomAttribute("max health additive bonus", 1200, 40.0); p.SetHealth(p.GetMaxHealth() + 1200); p.StunPlayer(25.0, 0.85, 1, null) }
	return blu
}

::Slay <- function(caption) {
	local blu = ::Arena(3, 2)
	::DoomCaption(caption, 3.5)
	if (!blu.len()) return
	local e = blu[0]
	SigfIn(0.4, function() { ::Aiming = true; SigfAimAt(e); SigfShoot(0.8) })
	SigfIn(1.5, function() { SigfKillByHost(e) })
	SigfIn(2.2, function() { ::Aiming = false })
}

SigfDemo(0.5, function() { ::Arena(4, 2); ::DoomCaption("DOOM TAKEOVER", 5) })
SigfDemo(5, function() { ::Slay("RIP AND TEAR!") })
SigfDemo(11, function() { ::Slay("MARINES VS IMPS AND CACODEMONS") })
SigfDemo(17, function() { ::Slay("BFG TIME!") })
SigfDemo(23, function() { ::Slay("KNEE DEEP IN THE DEAD") })
SigfDemo(29, function() { ::Slay("HELL YEAH!") })
SigfDemo(35, function() { ::Slay("RIP AND TEAR!") })
SigfDemo(41, function() { ::Slay("ONLY ONE THING LEFT TO DO") })
SigfDemo(47, function() { ::Slay("DOOM TAKEOVER") })
SigfDemo(53, function() { ::Slay("RIP AND TEAR!") })
SigfDemo(59, function() { ::Slay("BFG TIME!") })

// Keep the battle in front of the player: bots that wander off or respawn are pulled back into view.
::Keeper <- function() {
	local host = SigfHost()
	if (host == null || !host.IsAlive() || !::SigfShowReady) return
	if (::StageYaw < -900) return
	local r = ::StageYaw * 0.0174533
	local fwd = Vector(cos(r), sin(r), 0)
	local side = Vector(-sin(r), cos(r), 0)
	local n = 0
	foreach (p in SigfPlayers()) {
		if (p == host) continue
		local dd = (p.GetOrigin() - host.GetOrigin()).Length()
		if (dd < 520 && dd > 190) continue
		if (n >= 5) break
		n++
		local d = p.GetTeam() == 3 ? 230 + RandomInt(0, 70) : 400 + RandomInt(0, 80)
		::Place(p, fwd, side, d, RandomInt(-90, 90))
		p.AddCustomAttribute("max health additive bonus", 1200, 40.0)
		p.AddCustomAttribute("move speed bonus", 0.4, 40.0)
		p.SetHealth(p.GetMaxHealth() + 1200)
		p.StunPlayer(25.0, 0.85, 1, null)
	}
}
SigfEvery(1.5, function() { ::Keeper() })

// The pilot wanders; hold the player on the control point, looking at the battle.
SigfEvery(0.4, function() {
	local host = SigfHost()
	if (host == null || !host.IsAlive() || !::SigfShowReady || ::StageYaw < -900) return
	local cp = Entities.FindByClassname(null, "team_control_point")
	if (cp != null && (host.GetOrigin() - cp.GetOrigin()).Length() > 150) host.SetAbsOrigin(cp.GetOrigin() + Vector(0, 0, 40))
	if (!::Aiming) host.SnapEyeAngles(QAngle(4, ::StageYaw, 0))
})
