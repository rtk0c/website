---
title: "Setting up SJSU VPN for connection to home server"
draft: true
---

Note this intended for relative networking novices, so I will try to explain every term used. Skip over them if you find it verbose. If you don't care about anything else and just wants to replicate my setup for your home server, go to [this section](#my-journey). Read the TL;DR's in there if that section alone is too long for you too.

# Motivation
Virtual mesh networking software, like Tailscale, ZeroTier, tinc, Hamachi and else, practically[^1] cannot establish a direct/p2p connection between a machine on the SJSU wifi and a machine somewhere else, running on a common residential internet. This situation is an example of a hard-NAT to easy-NAT connnection (I'm using terminology from [Tailscale's article on NAT traversal](https://tailscale.com/blog/how-nat-traversal-works)). I really only use Tailscale.

Tailscale has an excellent relay service that can gaurentee _a_ connection between two machines even if it can't establish a direct connection. It has surprisingly good latency, mostly under 50ms for me going from SJSU wifi to a home server. But it has really limited bandwidth, on average 15Mbps based on a quick `iperf3` benchmark; this translates to about 1.2MB/s file transfer to my home server (from my experience), which isn't satisfactory for every task.

# Some Background
SJSU's network infrastructure works as follows (as of writing this, 2024-05-01):
- There are 2 wifi, `SJSU_Premier` and `SJSU_Guest` available to students and faculty.
    - The subnet is `10.0.0.0/8`. This means (inprecisely) every machine connected to the wifi will get a LAN IP address between `10.0.0.1` to `10.255.255.254`.
    - Both of them seems to be on the same subnet, i.e. machine A in `SJSU_Guest` can reach machine B in `SJSU_Premier` directly. This is based on my testing that joining to either one seems to allow connecting to another machine on the VPN.
- The gateway of the network is an endpoint-dependent firewall and endpoint-dependent NAT (this combination is what "hard NAT" describes). I assume this is some enterprise grade equipment from Cisco, though that's not super relevant.
- No IPv6 support whatsoever, both when connecting to the internet and inside the LAN.

SJSU also provides a VPN service based on the Cisco AnyConnect software. It is designed to be used for two purposes. First, like a traditional VPN: once you setup the client, all traffic is routed to become originated from SJSU's network; presumably to make some pay-walled text lending service available to who need it at home. Second, it allows you to reach any machine on the subnet `10.0.0.0/8` (both wifi's, as said above), in order to allow faculty to connect to services hosted only on the LAN[^2].

I wanted to utilize the second feature, to make Tailscale connect to my home server over "LAN" created by the VPN. For example if my home server had IP `10.0.12.1` from the VPN, my laptop will be able to connect by that IP directly. Tailscale will pick this up, avoiding having go through their relay.

My home server is running Linux. You can very much accomplish the same thing on Windows or macOS since Cisco provides VPN software for those too. You also won't need to jump through the hoops I did for Linux.

# My Journey
What I need to do is basically two things. (1) Setup Cisco Anyconnect on my home server. (2) Make it so that only the LAN subnet goes through the VPN, not all internet traffic. (I don't need to pretend, for example github.com, to be coming from SJSU's network). Number (2) is technically optional but a nice to have.

## Setting up the VPN
TL;DR: I used (`openconnect-sso`)[https://github.com/vlaci/openconnect-sso] on my browser to generate the VPN session token, and copy that to my home server over ssh, and launch OpenConnect with said token. This is because SJSU's account needs to authenticate with Okta/Duo, and that needs a browser.

I can either use Cisco's official Linux software, or use a 3rd-party, open source reimplementation like [OpenConnect](https://www.infradead.org/openconnect/). I _strongly_ prefered the latter since Cisco's official software wants me to download a blob of bash script to do installation, in addition to downloading another "Cisco Secure Desktop" executable from the internet, and running it locally on running.

TODO

## Un-route the internet from the VPN
TL;DR: use `ip route del default dev tun0` to get rid of the routing rule for all traffic, and then use `ip route add 10.0.0.0/8 dev tun0` to make the LAN subnet accesible.

TODO

# Results
TODO

# Closing Thoughts
I'm not sure if SJSU's Cisco Anyconnect service is going through another hop on a relay server of their own, or it's just a direct connection. I was more or less expecting the latency to be better than going through Tailscale's relay in SFO, though it is what it is.

I use ZeroTier for setting up game servers with my friends (advantage over Tailscale: no need for signing up an account). ZT doesn't not want to listen on the `10.xxx.yyy.zzz` address associated with the VPN, so even with the VPN in place, it still uses its own relay. I have no idea why, it could be its discovery mechanism (UDP local broadcast) is blocked by SJSU's network, or there is some kind of internal blacklist mechanism for blocking the `tun0` device used by OpenConnect. A quick github search in their source yield too many results for me to dig through; google did not hint at anything relevant.



[^1]: Some software like Tailscale have some heuristics to more-or-less brute force a direction connection between hard-NAT and easy-NAT. It takes quite a bit of luck for this to happen in my experience: for the close to 1 year I've been here, direction connection only ever happened once.

[^2]: "VPN allows users outside of the SJSU network access to restricted resources (like file shares, servers, and desktops) on the SJSU network, as if they are physically located on the SJSU campus network behind secured firewalls." https://sjsu.edu/it/services/network/internet-access/vpn.php
