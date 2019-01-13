#!/bin/bash
set -e

git config --global user.email "valentin.kasas@gmail.com"
git config --global user.name "vil1"
git config --global push.default simple

sbt microsite/publishMicrosite
