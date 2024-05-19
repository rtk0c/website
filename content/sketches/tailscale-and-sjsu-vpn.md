---
title: "Setting up SJSU VPN for connection to home server"
draft: true
---

Note this intended for relative networking novices, so I will try to explain every term used. Skip over them if you find it verbose. If you don't care about anything else and just wants to replicate my setup for your home server, go to [this section](#my-journey). Read the TL;DR's in there if that section alone is too long for you too.

# Motivation
Virtual mesh networking software, like Tailscale, ZeroTier, tinc, Hamachi and else, practically[^1] cannot establish a direct/p2p connection between a machine on the SJSU wifi and a machine somewhere else, running on a common residential internet. This situation is an example of a hard-NAT to easy-NAT connnection (I'm using terminology from [Tailscale's article on NAT traversal](https://tailscale.com/blog/how-nat-traversal-works)). I really only use Tailscale so that's what I'm concerned with here.

Tailscale has an excellent relay service that can gaurentee _a_ connection between two machines even if it can't establish a direct connection. It has surprisingly good latency, mostly under 50ms for me going from SJSU wifi to a home server. But it has really limited bandwidth, on average 15Mbps based on a quick `iperf3` benchmark; this translates to about 1.2MB/s file transfer to my home server (from my experience), which isn't satisfactory for every task.

# Some Background
SJSU's network infrastructure works as follows (as of writing this, 2024-05-01):
- There are 2 wifi, `SJSU_Premier` and `SJSU_Guest` available to students and faculty.
    - The subnet is `10.0.0.0/8`. This means, for our purposes, every machine connected to the wifi will get a Local-Area Network/*LAN* ("the wifi") IP address between `10.0.0.1` to `10.255.255.254`. A *subnet* is, for our purposes, just a range of IP address that all machines connected in a LAN will get their local IP address from.
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

- Install `openconnect-sso` using your method of choice. I got it from https://aur.archlinux.org/packages/openconnect-sso

- Run `openconnect-sso --server vpn.sjsu.edu --authgroup Student-SSO --user YOUR_SJSU_ID --authenticate`
  - Replace *YOUR_SJSU_ID* with, well, your SJSU ID (the 7 digit numbber)
  - The flag `--authenticate` tells it to only generate the session token, don't try to create a tunnnel.
  - This should print out something like
    ```
    HOST=https://vpn.sjsu.edu/
    COOKIE=<a very long hexdecimal string>
    FINGERPRINT=<a slightly shorter hexdecimal string>
    ```
  - From what I understood, `COOKIE` is Cisco Anyconnect's session token, which is only usable once. (That is to say, once you've connected to the VPN once with the step below, you need to do this current step again to get a new `COOKIE`.)

  - Then, go to your machine that you actually wish the VPN to run on. In my case, it's my personal server `ssh rtk0c@my-priv-server`

- Run, in place of the `...` copy paste the tokens you got from the last step
  ```sh
  $ export HOST=...
  $ export COOKIE=...
  $ export FINGERPRINT=...
  $ echo $COOKIE | sudo openconnect $HOST --cookie-on-stdin --servercert $FINGERPRINT
  ```
  You will leave the `openconnect` process running, since it is the VPN connection itself. i.e., it pushes wraps internet traffic and pushes them through an encrypted tunnel. Pass the token over stdin to avoid it lingering in the command line. Although it's not like it matters, since I control the whole server.

- Test with `curl http://icanhazip.com`, it should return an IP that belongs to SJSU. I got `130.65.9.242`.

## Un-route the internet from the VPN
TL;DR: use `ip route del default dev tun0` to get rid of the routing rule for all traffic, and then use `ip route add 10.0.0.0/8 dev tun0` to make the LAN subnet accesible.

`openconnect` automatically sets up a routing rule in the linux kernel that sends all internet traffic (i.e. every non-*private-use* IP address) *and* the subnet `10.0.0.0/8` through its *tunnel*, except those going to IP address of SJSU VPN server.

A *tunnel* manifests itself as a *network interface* in the linux kernel, in this case named `tun0`, just like a your WiFi card shows up as a network interface. Routing rules tell the kernel, when you see *packets* coming from such and such, and going to such and such IP address, send it through this network interface. A *private-use* IP address is one reserved by the IP standard, such that it will never appear on the internet. They're only used inside a LAN.

I want to get rid of the routing rules for all internet traffic. You can list routing rules with `ip route`[^ip-route], in which you should see something like:
```
default via 10.40.25.168 dev tun0
default via 192.168.1.1 dev wlp1s0 proto dhcp src 192.168.1.142 metric 600
10.40.16.0/20 dev tun0 scope link
130.65.9.242 via 192.168.1.1 dev wlp1s0 src 192.168.1.142 metric 600
130.65.9.242 via 192.168.1.1 dev wlp1s0 src 192.168.1.142 metric 20600
... rest are omitted ...
```

Each line here is a routing rule. They rules take priority from highest on top, to lowest on bottom. The first line, `default via 10.40.25.168 dev tun0`, means that if the destination IP address doesn't match anything below ("default"), send it to the device `tun0` ("dev tun0"). The 2nd line is the normal rule for my local WiFi connection (internet traffic goes to the router). The 3rd, 4th, and 5th lines all come from openconnect. 3rd says if the destination IP is in the `10.40.16.0/20` subnet, send it over `tun0`;  even if this rule didn't exist, packets going to the whole SJSU LAN subnet will be caught by the first rule, so it's unnecessary<sup>citation needed</sup>. 4th says if the destination IP is exactly `130.65.8.242`, which is SJSU's VPN sever, send it over my actual WiFi interface ("dev wlp1s0"); 5th is a duplicate but with a higher *metric*. I'm not sure why it writes these rules with so much redundency.

*Metric* is a number indicating the cost of a route. The higher this number, the less likely the kernel will consider it if other options exist.

In any case- all we need to do is get rid of the first line, and then add another rule to cover the whole `10.0.0.0/8` subnet (the current 3rd rule only covers a small section of the subnet). So we'll run:
```sh
$ sudo ip route del default dev tun0
$ sudo ip route add 10.0.0.0/8 dev tun0
```

Now test with `curl http://icanhazip.com` again. I got my normal, home IP address back! And test if SJSU's LAN subnet is reachable with `ping 10.0.0.1`. (I need a machine on the SJSU network, typically the ...1 machine is used by the router, I tried it, and indeed it exists—though I'm not sure what it is, but existence is all that matters).

# Script
I wrote a bash script `sjsu.vpn.sh`, to update the token I just copy paste them to the top of the file, as varaibles.

```bash
#! /bin/bash

HOST=https://vpn.sjsu.edu/
COOKIE=000 #your token
FINGERPRINT=000 #your fingerprint

# https://stackoverflow.com/a/1885534
read -p "Replacing the default everything route with 10.0.0.0/8 only route? (y/N)" REPLY
echo #Move to next line
if [[ $REPLY =~ ^[Yy]$ ]]
then
	ROUTE_LAN_ONLY=true
fi

echo $COOKIE | sudo openconnect $HOST --cookie-on-stdin --servercert $FINGERPRINT &

if [[ $ROUTE_LAN_ONLY = true ]]
then
	sudo ip route del default dev tun0 && sudo ip route add 10.0.0.0/8 dev tun0
fi

onexit() {
	kill $(jobs -p)
	if [[ $ROUTE_LAN_ONLY = true ]]
	then
		sudo ip route del 10.0.0.0/8 dev tun0
	fi
}

trap 'onexit' EXIT
wait
```

# Results
`iperf` (and `iperf3`) speed went from ~15Mbps on tailscale relay to ~55Mbps over the cisco vpn; ping didn't change meaningfully.

# Closing Thoughts
I'm not sure if SJSU's Cisco Anyconnect service is going through another hop on a relay server of their own, or it's just a direct connection. I was more or less expecting the latency to be better than going through Tailscale's relay in SFO, though it is what it is.

I use ZeroTier for setting up game servers with my friends (advantage over Tailscale: no need for signing up an account). ZT doesn't not want to listen on the `10.xxx.yyy.zzz` address associated with the VPN, so even with the VPN in place, it still uses its own relay. I have no idea why, it could be its discovery mechanism (UDP local broadcast) is blocked by SJSU's network, or there is some kind of internal blacklist mechanism for blocking the `tun0` device used by OpenConnect. A quick github search in their source yield too many results for me to dig through; google did not hint at anything relevant.



[^1]: Some software like Tailscale have some heuristics to more-or-less brute force a direction connection between hard-NAT and easy-NAT. It takes quite a bit of luck for this to happen in my experience: for the close to 1 year I've been here, direction connection only ever happened once.

[^2]: "VPN allows users outside of the SJSU network access to restricted resources (like file shares, servers, and desktops) on the SJSU network, as if they are physically located on the SJSU campus network behind secured firewalls." https://sjsu.edu/it/services/network/internet-access/vpn.php

[^ip-route]: Linux has the concept of different routing tables. `ip route` only shows the `main` routing table, but that's all we care about here. You can use `ip route show table <table name>` to show a specific table. Tailscale routes packets to the Tailnet IP addresses (the ones like 100.xxx.xxx.xxx) in the routing table `52`.
