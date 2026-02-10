# Generate facts
python ./main.py out.pl

# Run the Prolog `query_entry` goal and display markdown with glow
swipl -s out.pl -s queries.pl -g "query_entry()" -g halt | glow
