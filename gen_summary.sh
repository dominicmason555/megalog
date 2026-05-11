# Generate facts
uv run get_all

# Run the Prolog `query_entry` goal and display markdown with glow
swipl -s "$WELLKNOWN_SYNC_PERSONAL/facts/all.pl" -s queries.pl -g "query_entry()" -g halt | glow
