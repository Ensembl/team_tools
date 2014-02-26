#! /bin/sh

echo "Please note that database downtime was scheduled for 15:30.

This is an automatic reminder.  If the schedule was changed, please
ignore this mail.

-- 
Matthew's computer
" | mail \
    -a 'In-Reply-To: <53076108.1040302@sanger.ac.uk>' \
    -s 'Reminder: scheduled down time (otterlive:3324) at 15:30' \
    havana@sanger.ac.uk
