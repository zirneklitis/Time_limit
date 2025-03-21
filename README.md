# Time_limit

Version:
--------

0.10.00

In short:
---------

A windowed / full-screen countdown timer.
Colour and font size changes are used as warnings.
Progress-bar gives a glance at the time stream.
A logo can be added.
The logo can be used as a background as well.

Three different modes are available:

 - time left;
 - time passed;
 - ordinary clock.

When the time is over several possibilities are available:

 - to show the defined message;
 - to continue count the time;
 - to launch another application;
 - to close the count-down timer.


Useful for speech, lecture or presentation timing.

Colour / font / time limits can be changed using settings window and hot-keys.
It's possible to save and restore several configurations.
The input configuration file may be specified from command-line.

Available hot-keys:
-------------------
Hotkey | Purpose
--- | ---
[`Space`], [`s`], Mouse click | start/stop the timer.
[`F11`], [`f`], Mouse double-click  | Full screen on/off.
[`Esc`] | Exit.
[`b`] | Border-less on/off.
[`F1`], [`h`] | This help.
[`m`], Mouse right click | Menu window.
[`Right`], [`Up`] | Increase time by 10 seconds.
[`Left`], [`Down`] | Decrease time by 10 seconds.
[`Shift`]_[`Right`], [`Shift`]_[`Up`] | Increase time by 1 minute.
[`Shift`]_[`Left`], [`Shift`]_[`Down`] | Decrease time by 1 minute.
[`r`], Mouse middle click | reset timer.
[`c`] | Clock mode.
[`e`] | Elapsed time mode.
[`l`] | Remaining time mode.
[`t`] | Toggles „Always on top” and normal windows modes
[`v`] | Opens the timer's minimal console window.

- You can change current time with the mouse wheel as well.
- By clicking on progress-bar, the timer mode is toggled.

Available command-line parameters:
----------------------------------

Parameter | Purpose
--- | ---
`--config=`file.ini | Read the specified configuration file.
`-s` or `--start` | Starts the countdown.
`--run=`command | At the end of countdown launches another program.
`-e` or `--exit` | Exit at the end of countdown.
`-l` nn or `--lang` nn |Set the interface language (nn is the language code).
`-h` or `--help` | Show command-line help.

Availability:
-------------

 * License:
   - GPLv3

 * Programming Language:
   - Lazarus, Free Pascal

### Pre-compiled files

 * Sourceforge:
   - https://sourceforge.net/projects/time-limit/

 * Fedora Copr:
   - https://copr.fedorainfracloud.org/coprs/zirneklitis/Utilities/package/time_limit/
   - ``dnf copr enable zirneklitis/Utilities``


