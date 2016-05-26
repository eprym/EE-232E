edgefile = open("/Users/bairongjing/Dropbox/project_2_data/create/edge_weight.txt",'w')
moviefile = open("/Users/bairongjing/Dropbox/project_2_data/create/movie_list.txt",'w')
actorfile = open("/Users/bairongjing/Dropbox/project_2_data/create/merge_actor.txt",'r')
threshold = 5
actor_num_movie = dict()
movie_actor = dict()
movie_id = 0
edge_weight = dict()

# first part: create actor_num_movie dictionary: key is actor_id, value is # of movies of the actor_id
# also create movie_actor dictionary: key is movie_id, value is the movie_id + the list of actor_ids
for line in actorfile.readlines():
    line = line[:-1]
    tmp = line.split("\t\t")
    actor_num_movie[tmp[0]] = len(tmp) - 1
    for movie in tmp[1:]:
        if movie_actor.has_key(movie):
            if tmp[0] not in movie_actor[movie]:
                movie_actor[movie].append(tmp[0])
        else:
            movie_actor[movie] = list()
            movie_actor[movie].append(movie_id)
            movie_actor[movie].append(tmp[0])
            movie_id += 1

# second part: create moviefile which contains each movie with more than 5 actors in it
# also create edge_weight dictionary: key is the pair of two actor_ids, value is the number of same movies 
print "create edge_weight"
for row in movie_actor.items():
    value_movie_actor = row[1] 
    actor_num = len(value_movie_actor) - 1
    if actor_num >= threshold:
        moviefile.write(row[0])
        for tmp in value_movie_actor:
            moviefile.write("\t\t"+str(tmp))
        moviefile.write("\n")

    if actor_num > 1:
        for i in range(1,actor_num):
            for j in range(i+1,actor_num + 1):
                key1 = (int(value_movie_actor[i]),int(value_movie_actor[j]))
                key2 = (int(value_movie_actor[j]),int(value_movie_actor[i]))

                if edge_weight.has_key(key1):
                    edge_weight[key1] += 1
                    edge_weight[key2] += 1
                else:
                    edge_weight[key1] = 1
                    edge_weight[key2] = 1

# third part: caculate the actual edge weight, and write it into edgefile
print "calculate Edge weight"
for i in edge_weight.items():
    weight = float(i[1])/int(actor_num_movie[str(i[0][0])]);
    if weight > 1:
        print "this is impossible"
        continue
    edgefile.write(str(i[0][0]) + "\t\t" + str(i[0][1]) + "\t\t" + str(weight) + "\n")
   
edgefile.close()
moviefile.close()
print "finish the process of part 2"