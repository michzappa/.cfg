#!/bin/bash
CURRENT=`~/.config/polybar/keyboardlayout.sh`

if [ $CURRENT = 'us' ]
then
	setxkbmap -layout 'us(intl)'
fi

if [ $CURRENT = 'us(intl)' ]
then 
	setxkbmap -layout us
fi
