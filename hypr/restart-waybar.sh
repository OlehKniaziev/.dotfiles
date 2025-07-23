#!/bin/sh

pidof waybar | xargs kill
nohup waybar &
