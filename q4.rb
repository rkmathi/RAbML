#!/usr/bin/env ruby

### Command
# $ java -jar <<RAbML-0.0.1.jar>> <<train_ratings.dat>> <<test_ratings.dat>> lambda N

users = ['100', '200', '400', '800', '1600']
lambs = ['0.01', '0.05', '0.1', '0.5', '1']
ns    = ['2', '5', '10', '25', '50']

users.each { |u|
  lambs.each { |l|
    ns.each { |n|
      jar_path  = './RAbML-0.0.1.jar'
      train_path= "./dat/#{u}User_train_ratings.dat"
      test_path = "./dat/#{u}User_test_ratings.dat"

      print "#{u}, #{l}, #{n} => "

      result = `java -jar #{jar_path} #{train_path} #{test_path} #{l} #{n}`
      print "#{result}\n"
    }
  }
}

