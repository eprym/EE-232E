if __name__ == '__main__':
	outfile = open ("../project_2_data/top_100_director.txt",'w')
	read_top_250 = open("../project_2_data/top_250_movies.txt",'r')
	read_director = open ("../project_2_data/director_movies.txt",'r')
	movie_director = dict()
	print "Create movie_direct_map"
	for line in read_director.readlines():
		line = line[:-1]
		tmp=line.split("\t\t")
		for item in tmp[1:]:
			m = item.find(")")
			item = item[:m+1]
			item.strip(" ")
			item.strip("\t")
			movie_director[item] = tmp[0]
	read_director.close()

	count = 0
	num = 0
	print "create top 100 director"
	for line in read_top_250.readlines():
		line = line[:-1]
		line.strip(" ")
		line.strip("\t")
		if movie_director.has_key(line):
			# outfile.write(str(line) + "\t\t" + str(movie_director[line]) + "\n" )
			outfile.write(str(movie_director[line]) + "\n" )
			count += 1
		else:
			print line

	# read_top_250.close()
	print count
	outfile.close()
	print "Done"
