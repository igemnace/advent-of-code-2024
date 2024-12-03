.SUFFIXES:
.PHONY: all day01

# default command is phony that does nothing, for now
# add in compilation here once i add compiled langs
all:

# written in gleam!
day01:
	cd day01 && gleam run
