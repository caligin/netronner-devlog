# TODO bomb if not token in $1
curl -X POST http://localhost:8080/api/players/108105329232958151930/award_achievement -d 'achievement=Kan not' -H "Authorization: Bearer $1" -vvv
