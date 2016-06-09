if __name__ == '__main__':
	outfile = open ("../project_2_data/top_250_movies.txt",'w')
	readfile = open("../project_2_data/ratings_250.txt",'r')
	# count = 0
	for line in readfile.readlines():
		start = line.find(".")
		end = line.find(")")
		line=line[start+2:end+1].strip(" ")
		outfile.write(str(line)+"\n")
	outfile.close()
	print "Done"
 
