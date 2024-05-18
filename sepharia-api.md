
GET https://contextus.org/api/name/Lincoln%20sdf?ref_only=1
Returns:
```json
{
    "lang": "en",
    "is_ref": false,
    "completions": [
        "Abraham Lincoln, Poems by Lincoln (1846)",
        "Abraham Lincoln, Poems by Lincoln (1846), Poetry Written by Abraham Lincoln",
        "Abraham Lincoln, Poems by Lincoln (1846), The Bear Hunt"
    ],
    "completion_objects": [
        {
            "title": "Abraham Lincoln, Poems by Lincoln (1846)",
            "key": "Abraham Lincoln, Poems by Lincoln (1846)",
            "type": "ref",
            "is_primary": true,
            "order": 1000000
        },
        {
            "title": "Abraham Lincoln, Poems by Lincoln (1846), Poetry Written by Abraham Lincoln",
            "key": "Abraham Lincoln, Poems by Lincoln (1846), Poetry Written by Abraham Lincoln",
            "type": "ref",
            "is_primary": true,
            "order": 1000000
        },
        {
            "title": "Abraham Lincoln, Poems by Lincoln (1846), The Bear Hunt",
            "key": "Abraham Lincoln, Poems by Lincoln (1846), The Bear Hunt",
            "type": "ref",
            "is_primary": true,
            "order": 1000000
        }
    ]
}
```

POST https://contextus.org/api/index/Test_submission
form-data:
```http
json=%7B%22title%22%3A%22Test+submission%22%2C%22heTitle%22%3A%22Test+submission+hebrew%22%2C%22titleVariants%22%3A%5B%22Test+submission+alternate+title+1%22%5D%2C%22heTitleVariants%22%3A%5B%22Test+submission+alternate+title+1+hebrew%22%5D%2C%22categories%22%3A%5B%22The+Gilded+Age%22%5D%2C%22sectionNames%22%3A%5B%22house%22%2C%22room%22%2C%22corner%22%5D%7D
```
form-data-parsed:
```json
{
  "json": {
    "title": "Test submission",
    "heTitle": "Test submission hebrew",
    "titleVariants": [
      "Test submission alternate title 1"
    ],
    "heTitleVariants": [
      "Test submission alternate title 1 hebrew"
    ],
    "categories": [
      "The Gilded Age"
    ],
    "sectionNames": [
      "house",
      "room",
      "corner"
    ]
  }
}
```

POST https://contextus.org/api/texts/De_Lome_Letter_(1898)
form-data:
```http
json=%7B%22versionTitle%22%3A%22Sefaria+Community+Translation%22%2C%22versionSource%22%3A%22https%3A%2F%2Fwww.sefaria.org%22%2C%22text%22%3A%5B%22Test+paragraph+1%22%2C%22Test+paragraph+2%22%5D%2C%22language%22%3A%22en%22%7D
```
form-data-parsed:
```json
{
  "json": {
    "versionTitle": "Sefaria Community Translation",
    "versionSource": "https://www.sefaria.org",
    "text": [
      "Test paragraph 1",
      "Test paragraph 2"
    ],
    "language": "en"
  }
}
```

