# haskell-project

## How to compile the application

Run the command `stack build` in the terminal.

## How to execute the application

After compiling, run the command `stack exec haskell-project-exe` in the terminal.

## How to use the application (main functionality)

After executing, we will see the following information and start the interaction:

```
----------------------------------------------------
  Welcome to the Movie info app
  Please choose a query option below:
  (1) Download Data
  (2) Query by entering the park name
  (3) Query by entering the movie name
  (4) Query by entering both the park & movie name
  (5) Quit
----------------------------------------------------
```

1. Type `1` in the consoles to download and parse the XML data, and save it to the database:

```
Downloading...
Parsing...
Saving on DB...
Saved!
The XML is now ready for query processing
```

2. After it prints `The XML is now ready for query processing`, the user will be required to type `2`, `3` or `4` they interested in to get the event information. For example, type `3` and `Coco`:

```
3
Enter movie name > 
Coco
Looking for Coco events...
----On Tue, 2019-08-06T00:00:00, Coco will be screened in Lawler Park, 5210 W. 64th St.. The rating of the movie is PG. CC is provided. Please call (773) 284-7328 for more information.
----On Tue, 2019-08-06T00:00:00, Coco will be screened in West Lawn Park, 4233 W. 65th St.. The rating of the movie is PG. CC is provided. Please call (773) 284-2803 for more information.
...
```

3. Type `5` to quit:

```
Hope you've enjoyed using the app!
```

Note: If the user enters an invalid name they want to search, the application will give some error information. For example, after entering a park name `kkk` that is not listed in the database, the application will print `Couldn't find events for the given park name`.

## Details of the web source we are using

We use [Chicago Park District: Movies in the Parks 2019 - CKAN (data.gov)](https://catalog.data.gov/dataset/chicago-park-district-movies-in-the-parks-2019) to download the XML file. It is a list all Movies in the Parks events in Chicago in 2019.

The XML file looks like:

```xml
<response>
<row>
<row _id="row-4tmw.yqp4-zarr" _uuid="00000000-0000-0000-E34C-A9EF5A15E2DE" _position="0" _address="https://data.cityofchicago.org/resource/_7piw-z6r6/row-4tmw.yqp4-zarr">
<day>Mon</day>
<date>2019-07-08T00:00:00</date>
<park>Chippewa Park</park>
<park_phone>(773) 731-0380</park_phone>
<title>Bee Movie</title>
<cc>Y</cc>
<rating>PG</rating>
<underwriter>JCC Chicago</underwriter>
<park_address>6748 N. Sacramento Ave.</park_address>
<location latitude="42.00438076" longitude="-87.70475177"/>
</row>
...
```

## How we are extracting the information

1. We firstly download the file using `download ` function in `Fetch.hs`. 

2. In `Parse.hs`, we use [xeno](https://hackage.haskell.org/package/xeno) library to parse the XML to xeno.DOM.Node. Then we parse the Node data to Rows we defined in `Type.hs`.

## Extra features

- We have 3 tables (event, movie and park). The `movie` in the `event` table maps to the `title` in the `movie` table. The `park` in the `event` table maps to the `name` in the `park` table.
- We use XML-type data, which is more complex to parse. The external library we use has little documentation and no example.
- We write an instance of Show of the data type we query, so that it's more user-friendly when printing the results.
