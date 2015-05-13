TARGET=slides.html
MASTER=master.rst
SRCS=background.rst declarations.rst blocks.rst subroutine.rst module.rst
MATHML=mathml

default : $(TARGET)

$(TARGET) : $(MASTER) $(SRCS)
	rst2s5.py --link-stylesheet --math-output=$(MATHML) $< $@

clean : 
	rm -rf $(TARGET)

.PHONY : default clean
