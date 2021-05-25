
package nl.rivm.screenit;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
 * %%
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * =========================LICENSE_END==================================
 */

import java.math.BigDecimal;
import java.text.SimpleDateFormat;
import java.time.LocalTime;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import nl.rivm.screenit.util.ThreadLocalDateFormat;
import nl.topicuszorg.planning.model.enums.Days;

public class Constants
{
	public static final Locale LOCALE_NL = Locale.forLanguageTag("NL-nl");

	public static final int PASSWORDHASHINGITERATIONS = 300000;

	public static final int AANTAL_RIJEN = 20;

	public static final int AANTAL_PALETTE_RIJEN = 10;

	public static final int AANTAL_SPECIFIEKE_ZOEKRESULTATEN = 1;

	public static final int AANTAL_CLIENTEN_IN_SESSIE = 5;

	public static final int MAX_AUTO_PLAN_POGINGEN = 5;

	public static final EnumSet<Days> WERKDAGEN = EnumSet.range(Days.MONDAY, Days.FRIDAY);

	public static final String DEFAULT_DATE_FORMAT = "dd-MM-yyyy";

	public static final String DEFAULT_DATE_TIME_FORMAT = "dd-MM-yyyy HH:mm";

	public static final String DATE_FORMAT_YYYYMMDDHHMMSS = "yyyyMMddHHmmss";

	public static final String DATE_FORMAT_YYYYMMDDHHMM = "yyyyMMddHHmm";

	public static final String DATE_FORMAT_YYYYMMDD = "yyyyMMdd";

	public static final String DATE_FORMAT_DDMMYYY = "ddMMyyyy";

	public static final String JOB_PARAMETER_PREFIX = "SCREENIT_JOB_PARAM_";

	public static final String XML_PARAMETER_KOPPEL_JOB = JOB_PARAMETER_PREFIX + "koppelXMLData";

	public static final String CERVIX_ORDER_JOB_LABORATORIUM_PARAMETER = JOB_PARAMETER_PREFIX + "BMHKLabId";

	public static final String END_OF_TIME = "TO_DATE('01-01-4000', 'DD-MM-YYYY')";

	public static final String BEGIN_OF_TIME = "TO_DATE('01-01-1900', 'DD-MM-YYYY')";

	public static final String COOKIE_KEY_LOGIN_METHOD = "login.method";

	public static final String OID_EXTENSION_PATTERN = "^[0-2](\\.(0|[1-9][0-9]*))*\\.[0-9a-zA-Z\\-]*$";

	public static final double RADIANS = Math.PI / 180.0;

	public static final double EARTH_RADIUS = 6367000.0; 

	public static final String NO_OLD_CONCEPT = "-- no old concept --";

	public static final String RETOURZENDING_UITNODIGINGS_ID_MARKER = "cuid:";

	public static final String COLON_RETOURZENDING_MARKER = "retourzending";

	public static final String CERVIX_RETOURZENDING_MARKER = "cervix-retourzending";

	public static final String ALLEEN_VALIDATIE = "alleenValidatie";

	public static final String GBA_CHECK_ON_TIJDELIJK_ADRES_NU_ACTUEEL = "checkOnTijdelijkAdresNuActueel";

	public static final String GBA_ADRES_GEGEVENS_GEWIJZIGD = "adresGegevensGewijzigd";

	public static final String MAMMA_ADRES_GEWIJZIGD_MARKER = "|mammaTehuisOntkoppelen|";

	public static final String MAMMA_IMS_CLIENT_GEGEVENS_GEWIJZIGD_MARKER = "|mammaIMSGegevensGewijzigd|";

	public static final String MAMMA_IMS_CLIENT_BSN_GEWIJZIGD_MARKER = "mammaClientBSNGewijzigd";

	public static final String LOCATIEID = "LocatieID";

	private static final ThreadLocalDateFormat DATE_FORMAT = new ThreadLocalDateFormat(DEFAULT_DATE_FORMAT);

	private static final ThreadLocalDateFormat TIME_FORMAT = new ThreadLocalDateFormat("HH:mm");

	private static final ThreadLocalDateFormat DATE_TIME_FORMAT = new ThreadLocalDateFormat(DEFAULT_DATE_TIME_FORMAT);

	private static final ThreadLocalDateFormat DATE_DD_MM_YYYY_FORMAT = new ThreadLocalDateFormat(DATE_FORMAT_DDMMYYY);

	private static final ThreadLocalDateFormat DATE_TIME_SECONDS_FORMAT = new ThreadLocalDateFormat("dd-MM-yyyy HH:mm:ss");

	public static final String VRAAG_INCIDENT_COMPLICATIE_JA_NEE = "incident_complicatie_ja_nee";

	public static final String VRAAG_LAESIE_JA_NEE = "laesie_ja_nee_vraag";

	public static final String VRAAG_DATUM_VERRICHTING = "datum_verrichting_vraag";

	public static final LocalTime BK_EINDTIJD_DAG = LocalTime.of(21, 30);

	public static final int BK_TIJDVAK_MIN = 5;

	public static final BigDecimal BK_TIJDVAK_SEC = new BigDecimal(BK_TIJDVAK_MIN * 60);

	public static final int DEELNAMEKANSBEREKENING_NA_WEKEN = 15;

	public static final String EXCEPTION_GEBRUIKERMELDING_MARKER = "GEBRUIKERMELDING";

	public static final String CONTACT_EXTRA_PARAMETER_VANUIT_BK_PLANNING = "bkVanuitPlanning";

	public static final String CONTACT_EXTRA_PARAMETER_ALLEEN_CLIENT_CONTACT = "bkAlleenClientContact";

	public static final int BK_HA_BERICHT_MAX_WAITTME = 5;

	public static final int BK_HA_BATCH_BERICHTEN_AANMAAK_OUDER_DAN = 10;

	public static final String BASE_SUBDOMEIN_MEDEWERKERPORTAAL = "applicatie";

	private static Map<Boolean, String> booleanWeergave;

	public static final BigDecimal BK_MAXIMALE_TUMOR_GROOTTE = BigDecimal.valueOf(99.9);

	public static final String CDA_NULL_FLAVOR_VALUESET_NAME = "vs_null_flavor";

	public static final String HTTP_HEADER_X_CLIENT_CERT = "X-CLIENT-CERT";

	public static final String HTTP_HEADER_SSL_CLIENT_CERT = "SSL_CLIENT_CERT";

	public static final String BK_TNUMMER_ELEKTRONISCH = "T8888888";

	private Constants()
	{
	}

	static
	{

		booleanWeergave = new HashMap<Boolean, String>();
		booleanWeergave.put(Boolean.TRUE, "Ja");
		booleanWeergave.put(Boolean.FALSE, "Nee");
		booleanWeergave = Collections.unmodifiableMap(booleanWeergave);
	}

	public static Map<Boolean, String> getBooleanWeergave()
	{
		return booleanWeergave;
	}

	public static SimpleDateFormat getDateFormat()
	{
		return DATE_FORMAT.get();
	}

	public static SimpleDateFormat getTimeFormat()
	{
		return TIME_FORMAT.get();
	}

	public static SimpleDateFormat getDateTimeFormat()
	{
		return DATE_TIME_FORMAT.get();
	}

	public static SimpleDateFormat getDateTimeSecondsFormat()
	{
		return DATE_TIME_SECONDS_FORMAT.get();
	}

	public static SimpleDateFormat getDateDDMMYYYYFormat()
	{
		return DATE_DD_MM_YYYY_FORMAT.get();
	}

}
