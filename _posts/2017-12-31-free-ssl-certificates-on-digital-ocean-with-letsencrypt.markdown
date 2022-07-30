---
layout: post
title: 'Free SSL certificates on Digital Ocean with Letsencrypt'
date:   2017-12-31
categories: jekyll blog
---

Quick Start
---

Important Notes:
- Using Digital Ocean droplet 5$/month tier
- CentOS 7
- Apache (I am assuming this is already installed and running)
- May need sudo in front of these commands if you created a non-root user

1. Install EPEL ([Project Site](https://fedoraproject.org/wiki/EPEL)):
  ```
  wget https://dl.fedoraproject.org/pub/epel/epel-release-latest-7.noarch.rpm # may need `yum install wget`
  yum install epel-release-latest-7.noarch.rpm
  ```
2. Install `certbot` ([Project Site for CentOS/Apache](https://certbot.eff.org/#centosrhel7-apache)):
  ```
  yum install python-certbot-apache
  certbot --apache # follow prompts, I force SSL
  ```
3. Auto-renewal (because we are lazy, modified slightly from [Arch Wiki Let's
   Encrypt](https://wiki.archlinux.org/index.php/Let’s_Encrypt)):
    - The one-shot service which runs the renewal `/etc/systemd/system/certbot.service`:

            [Unit]
            Description=Let's Encrypt renewal
            
            [Service]
            Type=oneshot
            ExecStart=/usr/bin/certbot renew --quiet --agree-tos

    - The `certbot.service` will check for an expired certificate and install a new
      one only if necessary. Certbot recommends that you check twice a day with a
      random 60 minute delay, we do that with `/etc/systemd/system/certbot.timer`:

            [Unit]
            Description=Daily renewal of Let's Encrypt's certificates
            
            [Timer]
            Persistent=true
            OnBootSec=10min
            OnUnitActiveSec=12hour
            RandomizedDelaySec=1hour
            
            [Install]
            WantedBy=timers.target

    - Enable and start the timer:

            systemctl enable certbot.timer
            systemctl start certbot.timer

4. Shortly after posting this I noticed that URLs which don't exist yield an
   SSL error. I had to correct the vhost in
   `/etc/httpd/conf.d/le-redirect-www.chiroptical.com\:443.conf`. Just
   change the line: `ServerAlias www.chiroptical.com` to `ServerAlias
   www.chiroptical.com chiroptical.com` (obviously using your site name).
   This may have been because I already set this up previously using the
   non-`certbot` version of letsencrypt, but I don't remember where I did that.
    
Congratulations on your automatically renewed and free SSL certificates :)    
