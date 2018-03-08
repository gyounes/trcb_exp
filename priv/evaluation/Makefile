OS := $(shell uname)
ifeq ($(OS), Linux)
	VIEWER=eog
else ifeq ($(OS), Darwin)
	VIEWER=open
endif

all: transmission memory processing

pre: preprocess.py
	python preprocess.py

transmission: transmission.R pre
	Rscript transmission.R
	# $(VIEWER) *transmission*.png &
	$(VIEWER) *tcbcast*.png &

memory: memory.R pre
	Rscript memory.R
	$(VIEWER) *memory*.png &

latency: latency.R pre
	Rscript latency.R
	$(VIEWER) *latency*.png &

processing: processing.R pre
	Rscript processing.R
	$(VIEWER) *latency*.png &

transmission2: 
	Rscript transmission.R
	$(VIEWER) *tcbcast*.png &

memory2: 
	Rscript memory.R
	$(VIEWER) *memory*.png &

latency2: 
	Rscript latency.R
	$(VIEWER) *latency*.png &

processing2: 
	Rscript processing.R
	$(VIEWER) *latency*.png &

graphs: transmission2 memory2 processing2

clean:
	rm *.png
