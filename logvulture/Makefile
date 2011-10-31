
GROUP:=anacode-people
AGE=7
OLD_DIRLIST=$(shell ./list_logs.pl -d )
OLD_LOGSLIST=$(shell cat logs.txt )

interesting-bits.yaml: logs.txt Makefile peck_logfiles.pl
	./peck_logfiles.pl < $< > $@+ && mv $@+ $@

# Assumptions: code changes aren't relevant; logfiles are already
# listed; will run again soon (so update logs.txt for next time)
recent.yaml: $(OLD_LOGSLIST)
	@./peck_logfiles.pl $? </dev/null > $@+ && mv $@+ $@
	$(MAKE) logs.txt

logs.txt: Makefile list_logs.pl users.yaml $(OLD_DIRLIST)
	./list_logs.pl -a $(AGE) > $@+ && mv $@+ $@
	wc -l $@

users.yaml: Makefile list_users.pl
	./list_users.pl $(GROUP) > $@+ && mv $@+ $@


debug:
	@echo OLD_DIRLIST=$(OLD_DIRLIST)
