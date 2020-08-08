#!/usr/bin/env bash
CURRENT=`~/.scripts/get_current_keyboard_layout.sh`

if [ $CURRENT = 'US' ]
then
	setxkbmap -layout 'us(intl)'
fi

if [ $CURRENT = 'US(INTL)' ]
then 
	setxkbmap -layout us
fi
