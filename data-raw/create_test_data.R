# ── libraries ─────────────────────────────────────────────
library(tidyverse)
library(lubridate)
library(stringr)

# ── output folder ─────────────────────────────────────────
target_dir <- "data"
dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)

# ── global missing tokens ────────────────────────────────
miss_tokens <- c("NA", "NaN", "missing", "-999", "")

# ── helpers to random-case a word & dirty a column name ──
to_snake <- \(x) str_replace_all(str_to_lower(x), "\\s+", "_")
to_camel <- \(x) {
  w <- str_remove_all(str_to_title(x), "\\s+")
  paste0(str_to_lower(str_sub(w, 1, 1)), str_sub(w, 2))
}
random_case_word <- function(x) {
  switch(
    sample(1:5, 1),
    `1` = str_to_upper(x),
    `2` = str_to_lower(x),
    `3` = str_to_title(x),
    `4` = to_snake(x),
    `5` = to_camel(x)
  )
}
dirty_colname <- function(nm) {
  nm2 <- random_case_word(nm)
  if (runif(1) < 0.20) {
    nm2 <- paste0(" ", nm2, " ")
  } # lead/trail blanks
  if (runif(1) < 0.15) {
    nm2 <- paste0(nm2, sample(c("?", "/"), 1))
  } # add suffix
  nm2
}

# ── helper: randomise case of *values* ───────────────────
random_case <- function(vec) map_chr(vec, random_case_word)

# ── helpers to dirty up values ───────────────────────────
messy_cat <- function(x, pad_prob = 0.08, miss_prob = 0.12) {
  x <- random_case(x)
  pad <- runif(length(x)) < pad_prob
  x[pad] <- paste0(" ", x[pad], " ")
  miss <- runif(length(x)) < miss_prob
  x[miss] <- sample(miss_tokens, sum(miss), replace = TRUE)
  x
}
messy_num <- function(
  x,
  pound_prob = 0.20,
  comma_prob = 0.20,
  miss_prob = 0.15
) {
  v <- as.character(x)
  pound <- runif(length(v)) < pound_prob
  v[pound] <- paste0("£", v[pound])
  comma <- !pound & (runif(length(v)) < comma_prob)
  v[comma] <- format(as.numeric(v[comma]), big.mark = ",", scientific = FALSE)
  miss <- runif(length(v)) < miss_prob
  v[miss] <- sample(miss_tokens, sum(miss), replace = TRUE)
  v
}

# ── full theme specifications (15 categorical + 10 numeric) ──
theme_specs <- list(
  weather = list(
    cat = list(
      Condition = c(
        "Sunny",
        "Rainy",
        "Cloudy",
        "Snowy",
        "Windy",
        "Stormy",
        "Foggy",
        "Sleet"
      ),
      Region = c(
        "North",
        "South",
        "East",
        "West",
        "Midlands",
        "Coastal",
        "Inland"
      ),
      Season = c("Spring", "Summer", "Autumn", "Winter"),
      `Temperature Unit` = c("Celsius", "Fahrenheit", "Kelvin"),
      `Forecast Provider` = c(
        "Met Office",
        "NOAA",
        "AccuWeather",
        "Weather.com",
        "BBC"
      ),
      `Alert Level` = c("None", "Yellow", "Amber", "Red"),
      `Time of Day` = c("Morning", "Afternoon", "Evening", "Night"),
      `Cloud Cover` = c("Clear", "Partly Cloudy", "Mostly Cloudy", "Overcast"),
      `Wind Direction` = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"),
      `UV Index Category` = c(
        "Low",
        "Moderate",
        "High",
        "Very High",
        "Extreme"
      ),
      `Precip Type` = c("None", "Rain", "Snow", "Sleet", "Hail"),
      `Humidity Category` = c("Dry", "Comfortable", "Humid", "Oppressive"),
      `Visibility Band` = c(
        "Excellent",
        "Good",
        "Moderate",
        "Poor",
        "Very Poor"
      ),
      `Storm Name` = c(
        "Arwen",
        "Barra",
        "Corrie",
        "Dudley",
        "Eunice",
        "Franklin"
      ),
      `Day/Night` = c("Day", "Night")
    ),
    num = c(
      "Temperature °C",
      "Temperature °F",
      "Humidity %",
      "Wind Speed kph",
      "Rainfall mm",
      "Pressure hPa",
      "Visibility km",
      "Dewpoint °C",
      "FeelsLike °C",
      "Snowfall cm"
    )
  ),
  cats = list(
    cat = list(
      Breed = c(
        "Siamese",
        "Maine Coon",
        "Bengal",
        "Tabby",
        "Sphynx",
        "Persian",
        "Ragdoll",
        "British Shorthair"
      ),
      Colour = c(
        "Black",
        "White",
        "Orange",
        "Tortoiseshell",
        "Grey",
        "Calico",
        "Cream",
        "Brown"
      ),
      `Coat Length` = c("Short", "Medium", "Long", "Hairless"),
      Pattern = c("Solid", "Bi-colour", "Tabby", "Spotted", "Pointed"),
      `Eye Colour` = c("Blue", "Green", "Gold", "Copper", "Odd-eyed"),
      `Vaccination Status` = c(
        "Up-to-date",
        "Overdue",
        "Unknown",
        "Not Required"
      ),
      Microchipped = c("Yes", "No"),
      `Age Band` = c(
        "Kitten",
        "Junior",
        "Prime",
        "Mature",
        "Senior",
        "Geriatric"
      ),
      `Indoor/Outdoor` = c("Indoor", "Outdoor", "Both"),
      `Favourite Toy` = c(
        "Ball",
        "Laser Pointer",
        "Feather Wand",
        "Catnip Mouse",
        "String"
      ),
      Temperament = c("Friendly", "Shy", "Aggressive", "Playful", "Lazy"),
      `Food Preference` = c("Dry", "Wet", "Raw", "Mixed"),
      `Litter Type` = c("Clumping", "Non-Clumping", "Silica", "Recycled Paper"),
      `Adoption Source` = c("Shelter", "Breeder", "Stray", "Friend", "Online"),
      `Health Issue` = c("None", "Dental", "Obesity", "Allergy", "Arthritis")
    ),
    num = c(
      "Weight kg",
      "Age months",
      "Length cm",
      "Height cm",
      "Daily Calories",
      "Vet Visits",
      "Litter Used kg",
      "Play Sessions/week",
      "Naps/day",
      "Cost £/month"
    )
  ),
  cars = list(
    cat = list(
      Make = c(
        "Ford",
        "Toyota",
        "BMW",
        "Tesla",
        "Honda",
        "VW",
        "Audi",
        "Volvo"
      ),
      Model = c(
        "Focus",
        "Corolla",
        "320i",
        "Model 3",
        "Civic",
        "Golf",
        "A4",
        "S60"
      ),
      `Fuel Type` = c("Petrol", "Diesel", "Hybrid", "Electric", "Hydrogen"),
      Transmission = c("Manual", "Automatic", "CVT", "Dual-clutch"),
      `Body Style` = c("Hatchback", "Saloon", "SUV", "Estate", "Coupe"),
      Colour = c("Red", "Blue", "Black", "White", "Grey", "Silver"),
      `Drive Train` = c("FWD", "RWD", "AWD", "4WD"),
      `Trim Level` = c("Base", "Sport", "Luxury", "Premium", "Performance"),
      `Reg State` = c("New", "Used", "Imported", "Ex-lease"),
      `Emission Std` = c("Euro 4", "Euro 5", "Euro 6", "ULEZ"),
      Segment = c("A-City", "B-Supermini", "C-Compact", "D-Mid-size", "E-Exec"),
      `Ownership Type` = c("Private", "Fleet", "Hire", "Demo"),
      `Insurance Group` = as.character(1:50),
      `Warranty Status` = c("In-warranty", "Out-of-warranty", "Extended"),
      `Market Region` = c("UK", "EU", "US", "APAC", "MEA")
    ),
    num = c(
      "Engine cc",
      "Horsepower bhp",
      "Torque Nm",
      "CO₂ g/km",
      "Range km",
      "0-60 mph s",
      "Fuel Tank l",
      "Boot L",
      "Kerb Weight kg",
      "List Price £"
    )
  ),
  books = list(
    cat = list(
      Genre = c(
        "Sci-Fi",
        "Fantasy",
        "Romance",
        "Thriller",
        "History",
        "Biography"
      ),
      Format = c("Hardback", "Paperback", "eBook", "Audiobook"),
      Language = c("English", "Spanish", "French", "German", "Japanese"),
      Binding = c("Sewn", "Glue", "Spiral", "Stapled"),
      Audience = c("Adult", "YA", "Children"),
      Publisher = c(
        "Penguin",
        "HarperCollins",
        "Simon & Schuster",
        "Macmillan"
      ),
      Series = c("Yes", "No"),
      Edition = c("1st", "2nd", "3rd", "Revised"),
      Illustrated = c("Yes", "No"),
      `Award Winning` = c("Yes", "No"),
      `Bestseller Tier` = c("Top-10", "Top-100", "Long-tail"),
      `Age Group` = c("0-5", "6-12", "13-17", "18+"),
      `Region Setting` = c("Europe", "Asia", "Americas", "Africa", "Oceania"),
      `Author Nationality` = c("UK", "US", "Canada", "India", "Japan"),
      Signed = c("Yes", "No")
    ),
    num = c(
      "Pages",
      "Chapters",
      "Word Count",
      "Rating ⭐",
      "Print Run",
      "RRP £",
      "Discount %",
      "Stock Units",
      "Reprints",
      "Film Rights £"
    )
  ),
  movies = list(
    cat = list(
      Genre = c(
        "Action",
        "Drama",
        "Comedy",
        "Horror",
        "Documentary",
        "Animation"
      ),
      Rating = c("G", "PG", "12A", "15", "18", "R"),
      Language = c("English", "Spanish", "French", "Mandarin", "Hindi"),
      Format = c("2D", "3D", "IMAX", "Streaming"),
      Franchise = c("Standalone", "Franchise", "Reboot", "Spin-off"),
      `Production Studio` = c(
        "Warner",
        "Disney",
        "Universal",
        "Paramount",
        "Sony"
      ),
      `Release Season` = c("Spring", "Summer", "Autumn", "Winter"),
      `Budget Band` = c("<$10M", "$10-50M", "$50-150M", ">$150M"),
      `Distribution` = c("Cinema", "Streaming", "Direct-to-DVD", "Limited"),
      Sequel = c("Yes", "No"),
      `Award Noms` = c("0", "1-3", "4-7", "8+"),
      `Filming Location` = c("UK", "US", "Canada", "NZ", "Australia"),
      `Cinematography` = c("Digital", "Film", "IMAX 70mm"),
      `Lead Actor` = c("A-list", "B-list", "Newcomer"),
      `Sound Mix` = c("Dolby", "DTS", "Atmos")
    ),
    num = c(
      "Runtime min",
      "Box Office $M",
      "Metascore",
      "UserScore",
      "Screens",
      "Opening $M",
      "Marketing $M",
      "CGI Shots",
      "IMDb Votes",
      "Oscar Wins"
    )
  ),
  food = list(
    cat = list(
      Cuisine = c("Italian", "Chinese", "Mexican", "Indian", "Greek", "French"),
      `Meal Type` = c(
        "Breakfast",
        "Lunch",
        "Dinner",
        "Snack",
        "Dessert",
        "Brunch"
      ),
      `Dietary Tag` = c(
        "Vegan",
        "Vegetarian",
        "Gluten-Free",
        "Keto",
        "Halal",
        "Kosher"
      ),
      Course = c("Starter", "Main", "Side", "Drink", "Sweet"),
      `Spice Level` = c("Mild", "Medium", "Hot", "Fiery"),
      `Serving Temp` = c("Hot", "Cold", "Room Temp"),
      Allergen = c("Nuts", "Dairy", "Soy", "Gluten", "Egg", "Fish", "None"),
      Protein = c("Beef", "Chicken", "Pork", "Lamb", "Tofu", "Beans"),
      `Cooking Method` = c("Grilled", "Fried", "Baked", "Raw", "Steamed"),
      `Origin Country` = c(
        "Italy",
        "China",
        "Mexico",
        "India",
        "Greece",
        "France"
      ),
      Occasion = c("Holiday", "Birthday", "Casual", "Formal"),
      Packaging = c("Plastic", "Glass", "Paper", "Metal", "Compostable"),
      Organic = c("Yes", "No"),
      Fairtrade = c("Yes", "No"),
      `Restaurant Chain` = c(
        "Independent",
        "Franchise",
        "Street Food",
        "Fast Food",
        "Fine Dining"
      )
    ),
    num = c(
      "Calories kcal",
      "Fat g",
      "Protein g",
      "Carbs g",
      "Sugar g",
      "Salt g",
      "Cost £",
      "Serve Size g",
      "Prep Time min",
      "Cook Time min"
    )
  ),
  travel = list(
    cat = list(
      `Transport Mode` = c("Plane", "Train", "Car", "Bus", "Boat", "Bike"),
      Purpose = c(
        "Business",
        "Holiday",
        "Backpacking",
        "Study",
        "Relocation",
        "Event"
      ),
      Class = c("Economy", "Premium", "Business", "First"),
      `Ticket Type` = c("Return", "One-way", "Open"),
      `Booking Channel` = c("Online", "Agent", "App", "Walk-in"),
      `Airline/Carrier` = c("BA", "EasyJet", "Lufthansa", "Qatar", "Emirates"),
      `Loyalty Tier` = c("Bronze", "Silver", "Gold", "Platinum"),
      `Seat Preference` = c("Window", "Aisle", "Middle", "Bulkhead"),
      `Accom Type` = c("Hotel", "Hostel", "Airbnb", "Couchsurf", "Resort"),
      Continent = c(
        "Europe",
        "Asia",
        "N. America",
        "S. America",
        "Africa",
        "Oceania"
      ),
      `Visa Required` = c("Yes", "No"),
      Season = c("High", "Shoulder", "Low"),
      `Travel Insurance` = c("Basic", "Comprehensive", "None"),
      Currency = c("GBP", "USD", "EUR", "JPY", "AUD"),
      `Duration Band` = c("<3d", "3-7d", "1-2w", "2-4w", ">1m")
    ),
    num = c(
      "Distance km",
      "CO₂ kg",
      "Trip Cost £",
      "Days Away",
      "Stops",
      "Layover h",
      "Bags",
      "Delay min",
      "Travel Speed km/h",
      "Loyalty Points"
    )
  ),
  sports = list(
    cat = list(
      Sport = c(
        "Football",
        "Basketball",
        "Tennis",
        "Cricket",
        "Rugby",
        "Running"
      ),
      Surface = c("Grass", "Clay", "Hardcourt", "Indoor", "Synthetic", "Road"),
      `Competition Level` = c("Amateur", "Semi-Pro", "Pro", "International"),
      `Gender Cat` = c("Men", "Women", "Mixed"),
      Season = c("Regular", "Playoffs", "Pre-season"),
      `Weather Cond` = c("Dry", "Wet", "Windy", "Snowy", "Hot"),
      `Venue Type` = c("Outdoor", "Indoor"),
      `Team Name` = c("Lions", "Tigers", "Bears", "Sharks", "Eagles", "Wolves"),
      Coach = c("Veteran", "Rookie", "Interim"),
      League = c("A-League", "B-League", "C-League"),
      `Equipment Brand` = c("Nike", "Adidas", "Puma", "Wilson"),
      `Scoring Method` = c("Points", "Goals", "Runs", "Sets"),
      `Time Slot` = c("Morning", "Afternoon", "Evening", "Night"),
      `Official Count` = c("1", "2", "3", "4", "5"),
      `Broadcast Net` = c("ESPN", "Sky", "BBC", "NBC")
    ),
    num = c(
      "Attendance",
      "Ticket £",
      "Game Time min",
      "Possession %",
      "Fouls",
      "Shots",
      "Pass Accuracy %",
      "Yards",
      "Errors",
      "Sponsorship £k"
    )
  ),
  music = list(
    cat = list(
      Genre = c("Rock", "Pop", "Jazz", "Classical", "Hip-hop", "EDM"),
      Format = c("Streaming", "Vinyl", "CD", "Cassette", "Live"),
      Language = c("English", "Spanish", "Korean", "French", "German"),
      Mood = c("Happy", "Sad", "Energetic", "Chill", "Angry"),
      Decade = c("70s", "80s", "90s", "2000s", "2010s", "2020s"),
      Label = c("Sony", "Universal", "Warner", "Indie"),
      Explicit = c("Yes", "No"),
      `Chart Pos Band` = c("Top-10", "Top-40", "Top-100", "Uncharted"),
      `Instrument Focus` = c("Guitar", "Piano", "Synth", "Drums", "Strings"),
      `Live/Studio` = c("Live", "Studio"),
      `Grammy Won` = c("0", "1-3", "4-7", "8+"),
      Remastered = c("Yes", "No"),
      `Playlist Type` = c("Mood", "Workout", "Focus", "Party", "Sleep"),
      `Audience Age` = c("Kids", "Teens", "Adults", "Seniors"),
      `Region Popularity` = c("US", "UK", "EU", "Asia", "LATAM")
    ),
    num = c(
      "Streams M",
      "Sales k",
      "Listeners k",
      "Peak Pos",
      "Weeks Charted",
      "Tour Gross £M",
      "Track Length s",
      "BPM",
      "Key",
      "Energy %"
    )
  ),
  technology = list(
    cat = list(
      Device = c("Laptop", "Smartphone", "Tablet", "Server", "Drone", "Robot"),
      `Operating System` = c(
        "Windows",
        "macOS",
        "Linux",
        "Android",
        "iOS",
        "ChromeOS"
      ),
      `Processor Brand` = c("Intel", "AMD", "Apple", "Qualcomm"),
      `Form Factor` = c("Ultrabook", "2-in-1", "Desktop", "Mini PC"),
      `Release Yr Band` = c("<2015", "2015-17", "2018-20", "2021-23", "2024+"),
      `Market Segment` = c("Consumer", "Enterprise", "Education", "Gaming"),
      Manufacturer = c("Dell", "HP", "Lenovo", "Apple", "Asus", "Acer"),
      Connectivity = c("Wi-Fi", "5G", "Ethernet", "Bluetooth"),
      `Screen Type` = c("LCD", "OLED", "Mini-LED", "E-Ink"),
      `Energy Rating` = c("A", "B", "C", "D"),
      `Warranty Length` = c("1y", "2y", "3y", "5y", "No Warranty"),
      Colour = c("Black", "Silver", "Grey", "White", "Blue"),
      `Price Tier` = c("Budget", "Mid-range", "Premium", "Flagship"),
      `User Profile` = c("Student", "Professional", "Creator", "Gamer"),
      `Upgrade Elig` = c("Yes", "No")
    ),
    num = c(
      "Price £",
      "RAM GB",
      "SSD GB",
      "Battery mAh",
      "Weight g",
      "Ports",
      "CPU GHz",
      "GPU TFLOPS",
      "Launch Score",
      "Units Sold k"
    )
  )
)

# ── dataset generator ───────────────────────────────────
make_dataset <- function(theme, n = 1000) {
  spec <- theme_specs[[theme]]

  cat_cols <- imap_dfc(spec$cat, \(choices, nm) {
    tibble(!!nm := messy_cat(sample(choices, n, replace = TRUE)))
  })

  num_cols <- imap_dfc(spec$num, \(nm, i) {
    raw <- sample(1:20000, n, replace = TRUE)
    tibble(!!nm := messy_num(raw))
  })

  dates <- sample(
    seq.Date(ymd("2021-01-01"), ymd("2024-12-31"), by = "day"),
    n,
    replace = TRUE
  )
  date_str <- case_when(
    runif(n) < 0.4 ~ format(dates, "%Y-%m-%d"),
    runif(n) < 0.8 ~ format(dates, "%d/%m/%Y"),
    TRUE ~ format(dates, "%b %d, %Y")
  )
  miss <- runif(n) < 0.08
  date_str[miss] <- sample(miss_tokens, sum(miss), replace = TRUE)

  df <- tibble(Date = date_str) |>
    bind_cols(cat_cols, num_cols) |>
    slice_sample(n = n)

  # ── one-liner to mess up the column names ───────────────
  names(df) <- make.unique(map_chr(names(df), dirty_colname), sep = "_dup")
  df
}

# ── write the files ─────────────────────────────────────
set.seed(789)

walk(names(theme_specs), \(thm) {
  df <- make_dataset(thm)
  write_csv(
    df,
    file.path(target_dir, paste0(thm, ".csv")),
    na = "",
    quote = "needed"
  )
  message("✅  Wrote ", thm, ".csv with ", ncol(df), " columns → ", target_dir)
})
