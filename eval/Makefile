OUT=../out


all: $(OUT)/kkj


$(OUT)/kkj: main.hs compile.hs core.hs
	mkdir -p $(OUT)
	ghc -outputdir=$(OUT) main.hs -o $(OUT)/kkj


clean:
	rm -f $(OUT)/*.hi
	rm -f $(OUT)/*.o
	rm -f $(OUT)/kkj
	rmdir $(OUT)


.PHONY: all clean

