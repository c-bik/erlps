# erlps
OS process controller

#### Suppored OS
Currently being tested on Fedora and Windows 7

wmic (windows) and ps (linux) commands are used in this appication. So, theoritically, this application should work on all OSs supporting either of these two commands.

#### Usage
```erlang
erlps:list(). % Current process name and pid
```
