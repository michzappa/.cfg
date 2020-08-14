#!/usr/bin/env bash
CURRENT=`~/.scripts/get_current_keyboard_layout.sh`

if [ $CURRENT = 'US' ]
then
	setxkbmap -layout 'us(alt-intl)'
fi

if [ $CURRENT = 'US(ALT-INTL)' ]
then 
	setxkbmap -layout us
fi
