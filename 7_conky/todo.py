#!/usr/bin/python

# Read Calcurse
#a=open("/home/chew/.calcurse/todo","r")
#lines=a.readlines()
#i=0
#for line in lines:
#    i+=1
#    if(len(line)>=45):
#        print("%d. " %i + line[4:45], end="\n")
#    else:
#        print("%d. " %i + line[4:45], end="")
#
#    if i==5:
#        break

# Read Text File
a=open("/home/chew/.todo.txt","r")
lines=a.readlines()
i=0
for line in lines:
    i+=1
    if(len(line)>=45):
        print("%d. " %i + line[0:45], end="\n")
    else:
        print("%d. " %i + line[0:45], end="")

    if i==5:
        break

