
writefile = open( "/Users/bairongjing/Dropbox/project_2_data/create/merge_actor.txt",'w')
actorfile = open( "/Users/bairongjing/Dropbox/project_2_data/actor_movies.txt",'r')
actressfile = open("/Users/bairongjing/Dropbox/project_2_data/actress_movies.txt",'r')
threshold = 5
id= 0
#first process the date in actor_movies.txt
for line in actorfile.readlines():
    #print id, line
    line = line[:-1]
    tmp=line.split("\t\t")
    if len(tmp)< threshold+1:
        continue
    writefile.write(str(id))
    id+=1
    for i in tmp[1:]:
        end = i.find(")")
        i = i[:end+1]
        i.strip(" ")
        i.strip("\t")
        writefile.write("\t\t")
        writefile.write(str(i))
    writefile.write("\n")
print"actor"
#second process the date in actress_movies.txt
for line in actressfile.readlines():
    # print id, line
    line = line[:-1]
    tmp=line.split("\t\t")
    if len(tmp)<threshold+1:
        continue
    writefile.write(str(id))
    id+=1
    for i in tmp[1:]:
        end = i.find(")")
        i = i[:end+1]
        i.strip(" ")
        i.strip("\t")
        writefile.write("\t\t")
        writefile.write(str(i))
    writefile.write("\n")
writefile.close()
print"actress"
