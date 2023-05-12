# Configuration file for forecast_script_internal.Rmd
# forecast_script_internal.Rmd -ennustimen asetustiedosto

# Where does your raw data come from? Currently, raw data looks like this: DWSALES_YYYY-MM-DDT######.dat
# Missä ennustimen raakadata sijaitsee? Raakadata näyttää toistaiseksi tältä: DWSALES_YYYY-MM-DDT######.dat
INPUT <- "~/prodfore_internal/data/2022/"

# Where do you want to save the aggregate data it needs?
# Minne ennustimen halutaan tallentavan sen tarvitsemia aggregaattitietoja?
OUTPUT <- "~/prodfore_internal/data/"

# Should we run the analysis for the Economics Department (or the more short term, operational dept.)
# Ajetaanko "vain" talousosaston tarvitsemat ennusteet (vai myös lyh. aikavälin operationaaliset)
ECON <- TRUE

# Should we impute for the COVID DIP 2020? CONSIDER THRICE BEFORE ENABLING!!
# Pitääkö dataa korjailla COVID-dippauksen 2020 osalta? HARKITSE TARKKAAN!!
FIX <- TRUE

if (ECON) {
    # What time resolution do we want? E.g. daily, weekly, monthly, yearly.
    # Millä aikaresoluutiolla ennusteet halutaan? Esim. päivittäinen, viikoittainen, kuukausittainen, vuosittainen.
    RES <- "monthly"

    # How many years of data should the forecasting methods see? More data often better but may hinder the ability to adapt to changes!
    # IN YEARS
    # Kuinka monta vuotta ennustinmenetelmät näkevät? Laajempi datasetti on usein parempi, mutta pitkä aikajakso saattaa hidastaa ennustimen kykyä reaoida muutoksiin!
    # VUOSINA
    TRAIN_LEN <- as.integer(5)

    # Length of the period the available methods should be tested against (so they can be selected!)
    # IN MONTHS
    # Mallinvalinnan testijakson pituus
    # KUUKAUSINA
    TEST_LEN <- as.integer(12)

    # How far should we forecast? In months.
    # Kuinka pitkälle ennuste yltää? Kuukausina.
    HORIZON <- as.integer(72)

    # Which products should we report on?
    # Possible: RBC, PLAT, O+, O-, A+, A-, B+, B-, AB+, AB-
    # Mitkä kaikki tuotteet ennusteeseen halutaan?
    # Mahdolliset: RBC, PLAT, O+, O-, A+, A-, B+, B-, AB+, AB-
    RBC <- TRUE
    PLAT <- TRUE
    # NB! Typed product histories do not extend to 2004, so they will not be available for economic fcasts.
    OPLUS <- FALSE
    OMINUS <- FALSE
    APLUS <- FALSE
    AMINUS <- FALSE
    BPLUS <- FALSE
    BMINUS <- FALSE
    ABPLUS <- FALSE
    ABMINUS <- FALSE


} else {
    # What time resolution do we want? E.g. daily, weekly, monthly, yearly.
    # Millä aikaresoluutiolla ennusteet halutaan? Esim. päivittäinen, viikoittainen, kuukausittainen, vuosittainen.
    RES <- "weekly"

    # How many years of data should the forecasting methods see? More data often better but may hinder the ability to adapt to changes!
    # IN YEARS
    # Kuinka monta vuotta ennustinmenetelmät näkevät? Laajempi datasetti on usein parempi, mutta pitkä aikajakso saattaa hidastaa ennustimen kykyä reaoida muutoksiin!
    # VUOSINA
    TRAIN_LEN <- as.integer(3)

    # Length of the period the available methods should be tested against (so they can be selected!)
    # IN MONTHS
    # Mallinvalinnan testijakson pituus
    # KUUKAUSINA
    TEST_LEN <- as.integer(12)

    # How far should we forecast? In months.
    # Kuinka pitkälle ennuste yltää? Kuukausina.
    HORIZON <- as.integer(1)

    # Which products should we report on?
    # Possible: RBC, PLAT, O+, O-, A+, A-, B+, B-, AB+, AB-
    # Mitkä kaikki tuotteet ennusteeseen halutaan?
    # Mahdolliset: RBC, PLAT, O+, O-, A+, A-, B+, B-, AB+, AB-
    RBC <- TRUE
    PLAT <- TRUE
    OPLUS <- FALSE
    OMINUS <- FALSE
    APLUS <- FALSE
    AMINUS <- FALSE
    BPLUS <- FALSE
    BMINUS <- FALSE
    ABPLUS <- FALSE
    ABMINUS <- FALSE

}
