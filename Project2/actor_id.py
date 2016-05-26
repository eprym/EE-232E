
idfile = open("/Users/bairongjing/Dropbox/project_2_data/create/actor_id.txt",'w')
actorfile = open("/Users/bairongjing/Dropbox/project_2_data/actor_movies.txt",'r')
actressfile = open("/Users/bairongjing/Dropbox/project_2_data/actress_movies.txt",'r')

threshold = 5
id = 0
for line in actorfile.readlines():
    # print id, line
    line = line[:-1]
    tmp=line.split("\t\t")
    # print tmp
    if len(tmp)< threshold+1:
        continue
    tmp[0].strip(" ")
    tmp[0].strip("\t")
    idfile.write(str(tmp[0]) + "\t\t" + str(id))
    id +=1
    idfile.write("\n")

print "Finish id of actor"
for line in actressfile.readlines():
    # print id, line
    line = line[:-1]
    tmp=line.split("\t\t")
    # print tmp
    if len(tmp)< threshold+1:
        continue
    tmp[0].strip(" ")
    tmp[0].strip("\t")
    idfile.write(str(tmp[0]) + "\t\t" + str(id))
    id +=1
    idfile.write("\n")
idfile.close()
print "Finish id of actress"