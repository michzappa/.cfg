#!/bin/bash
CURRENT=`~/.scripts/get_current_keyboard_layout.sh`

if [ $CURRENT = 'us' ]
then
	setxkbmap -layout 'us(intl)'
fi

if [ $CURRENT = 'us(intl)' ]
then 
	setxkbmap -layout us
fi
