---
title: "Adventures on server setup without a monitor"
date: 2024-11-18T21:41:35-08:00
tags: ["networking"]
ShowToc: false
---

A story of frantically rescuing a deployed headless server, where I forget to statically assign an IP address. 
It just won't have network connection. No SSH. No fixie.

Except _there is nothing saying a DHCP server has to be running on the router or gateway_.
So, just install and configure `kea` (or a DHCP server of your choice) on any other computer on the network, reboot the server in question, and voilà!
SSH to your hearts content.

---

So the story goes like this.
I recently got hands on a nice little old desktop tower, plenty of RAM and a good number of SATA ports for selfhosting: Seafile, Immich and what not.
Now it _is_ rather old machine, with only VGA and DVI on the motherboard. This means my little trusty HDMI to USB video capture dongle won't be helpful!
I also need to deploy this machine to my parent's house, for I plan on giving them access to a photo backup solution.
I need to bring a monitor that has a VGA port on it.

So I at the comfort of my home, I took some time to install Debian, as well as all other packages I could possibly need. Also configured a SSH key.

And as all good stories go, the _one thing_ I was supposed to do was not done. I didn't assign a static IP to `eth0` beforehand.

_Uh. oh._

There is no proper DHCP in the lan, I had everything else setup with static IPs. I don't know its MAC, so no calculating SLAAC by hand to get a link local IPv6 either. In fact, if I remember correctly I don't even think the gateway was properly setup with IPv6 at all.

No network, no SSH.

No monitor, no way to see what's happening on screen to configure its WiFi connection properly.

Quickly, I thought "what if I could just type out all the commands without a monitor?"
Trying to open vim on `/etc/network/interfaces` and blindly modifying a complex configuration file obviously did not work so well.
Although I did figure out one helpful tidbit: since this machine has a beeper, I can run things like `foobar && tput bel` in TTY to get an audio confirmation that something succeeded.

After ten minutes of desparately trying various commands and questioning if I had been making typos all along, an enlightenment suddenly found its way into my mind: nothing is stopping me from running an ad-hoc DHCP server just for this purpose!
As far as I know, all of the networking implementations shipped by various Linux distros default to DHCP. This includes `NetworkManager`, `systemd-networkd`, or even Debian's default networking setup.

[Kea](https://www.isc.org/kea/) is apparently the recommended implementation, so I installed it on my laptop, and after some fiddling of configs per the [ArchWiki](https://wiki.archlinux.org/title/Kea) (because ArchWiki is the one wiki to rule them all), it did work. The server allocates the first address in the pool, so I just picked `192.168.233.1/16` and successfully SSH-ed in.

No need to drive 30 minutes round trip to get my VGA monitor!
