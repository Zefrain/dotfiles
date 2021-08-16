#!/usr/bin/env python3
# -*- coding: utf-8 -*-

def PEG(PE, expectation, current):
    return (PE / (((expectation/current) ** (1/3) - 1) * 100))


PE = float(input("current PE: "))
expectation = float(input("expectation profit after 3y: "))
current = float(input("current profit:"))
print("PEG:", PEG(PE, expectation, current))
