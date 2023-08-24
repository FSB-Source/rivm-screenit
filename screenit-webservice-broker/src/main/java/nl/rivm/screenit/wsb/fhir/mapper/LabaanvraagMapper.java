package nl.rivm.screenit.wsb.fhir.mapper;

/*-
 * ========================LICENSE_START=================================
 * screenit-webservice-broker
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.model.cervix.CervixLabformulier;
import nl.rivm.screenit.wsb.fhir.resource.dstu3.v1.LabaanvraagBundle;

import org.apache.commons.lang.StringUtils;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Named;
import org.mapstruct.ReportingPolicy;
import org.mapstruct.factory.Mappers;

@Mapper(unmappedTargetPolicy = ReportingPolicy.WARN)
public interface LabaanvraagMapper
{

	LabaanvraagMapper INSTANCE = Mappers.getMapper(LabaanvraagMapper.class);

	@Mapping(source = "scanDatum", target = "scanDatum")
	@Mapping(
		source = "uitstrijkje.monsterId",
		target = "barcode",
		qualifiedByName = "getTrimmedAnswer")
	@Mapping(
		source = "objid",
		target = "objid",
		qualifiedByName = "getTrimmedLowercaseAnswer")
	@Mapping(source = "laboratorium", target = "laboratorium")
	@Mapping(source = "uitstrijkje", target = "uitstrijkje")
	@Mapping(source = "huisartsLocatie", target = "huisartsLocatie")
	@Mapping(source = "status", target = "status")
	@Mapping(source = "statusDatum", target = "statusDatum")
	@Mapping(target = "digitaal", constant = "true")
	@Mapping(
		source = "datumUitstrijkje",
		target = "datumUitstrijkje",
		qualifiedByName = "stringToDate")
	@Mapping(
		source = "klachten",
		target = "klachtenGeen",
		qualifiedByName = "isEmpty")
	@Mapping(
		source = "klachten",
		target = "klachtenContactbloedingen",
		qualifiedByName = "klachtenContactbloedingen")
	@Mapping(
		source = "klachten",
		target = "klachtenKlachtenVanAbnormaleFluorZonderDuidelijkeOorzaak",
		qualifiedByName = "klachtenKlachtenVanAbnormaleFluorZonderDuidelijkeOorzaak")
	@Mapping(
		source = "klachten",
		target = "klachtenIntermenstrueelBloedverlies",
		qualifiedByName = "klachtenIntermenstrueelBloedverlies")
	@Mapping(
		source = "klachten",
		target = "klachtenPostmenopauzaalBloedverlies",
		qualifiedByName = "klachtenPostmenopauzaalBloedverlies")
	@Mapping(
		source = "klachten",
		target = "klachtenAndersNamelijk",
		qualifiedByName = "klachtenAndersNamelijk")
	@Mapping(
		source = "klachtenVrijeTekst",
		target = "klachtenAndersNamelijkTekst",
		qualifiedByName = "getTrimmedAnswer")
	@Mapping(
		source = "menstruatie",
		target = "menstruatieNormaal",
		qualifiedByName = "menstruatieNormaal")
	@Mapping(
		source = "menstruatie",
		target = "menstruatieGeenMenstruatie",
		qualifiedByName = "menstruatieGeenMenstruatie")
	@Mapping(
		source = "menstruatie",
		target = "menstruatieMenopauze",
		qualifiedByName = "menstruatieMenopauze")
	@Mapping(
		source = "menstruatie",
		target = "menstruatiePostmenopauze",
		qualifiedByName = "menstruatiePostmenopauze")
	@Mapping(
		source = "datumLaatsteMenstruatie",
		target = "datumLaatsteMenstruatie",
		qualifiedByName = "stringToDate")
	@Mapping(
		source = "anticonceptie",
		target = "anticonceptieGeen",
		qualifiedByName = "anticonceptieGeen")
	@Mapping(
		source = "anticonceptie",
		target = "anticonceptiePil",
		qualifiedByName = "anticonceptiePil")
	@Mapping(
		source = "anticonceptie",
		target = "anticonceptieIudKoper",
		qualifiedByName = "anticonceptieIudKoper")
	@Mapping(
		source = "anticonceptie",
		target = "anticonceptieIudMirena",
		qualifiedByName = "anticonceptieIudMirena")
	@Mapping(
		source = "anticonceptie",
		target = "anticonceptieAnders",
		qualifiedByName = "anticonceptieAnders")
	@Mapping(
		source = "gebruikHormonen",
		target = "gebruikHormonenJaVanwegeOvergangsklachten",
		qualifiedByName = "gebruikHormonenJaVanwegeOvergangsklachten")
	@Mapping(
		source = "gebruikHormonen",
		target = "gebruikHormonenJaVanwegeBorstkanker",
		qualifiedByName = "gebruikHormonenJaVanwegeBorstkanker")
	@Mapping(
		source = "gebruikHormonen",
		target = "gebruikHormonenJaVanwege",
		qualifiedByName = "gebruikHormonenJaVanwege")
	@Mapping(
		source = "gebruikHormonenVrijeTekst",
		target = "gebruikHormonenJaVanwegeTekst",
		qualifiedByName = "getTrimmedAnswer")
	@Mapping(
		source = "gebruikHormonen",
		target = "gebruikHormonenGeen",
		qualifiedByName = "isEmpty")
	@Mapping(
		source = "aspectCervix",
		target = "aspectCervixNormaal",
		qualifiedByName = "aspectCervixNormaal")
	@Mapping(
		source = "aspectCervix",
		target = "aspectCervixAbnormaalOfVerdachtePortio",
		qualifiedByName = "aspectCervixAbnormaalOfVerdachtePortio")
	@Mapping(source = "opmerkingenTekst", target = "opmerkingen", qualifiedByName = "isNotBlank")
	@Mapping(
		source = "opmerkingenTekst",
		target = "opmerkingenTekst",
		qualifiedByName = "getTrimmedAnswer")
	CervixLabformulier toLabformulier(LabaanvraagBundle bundle);

	@Named("stringToDate")
	static Date stringToLabformulierDate(String string) throws ParseException
	{
		final SimpleDateFormat simpleDateFormat = new SimpleDateFormat(Constants.DATE_FORMAT_YYYYMMDD);
		simpleDateFormat.setLenient(false);
		return string != null
			? simpleDateFormat.parse(string)
			: null;
	}

	@Named("isEmpty")
	static boolean isEmpty(List<String> list)
	{
		return list.isEmpty();
	}

	@Named("isNotBlank")
	static boolean isNotBlank(String string)
	{
		return StringUtils.isNotBlank(string);
	}

	@Named("klachtenContactbloedingen")
	static boolean klachtenContactbloedingen(List<String> list)
	{
		return list.contains("contactbloedingen");
	}

	@Named("klachtenKlachtenVanAbnormaleFluorZonderDuidelijkeOorzaak")
	static boolean klachtenKlachtenVanAbnormaleFluorZonderDuidelijkeOorzaak(List<String> list)
	{
		return list.contains("klachten van abnormale fluor zonder duidelijke oorzaak");
	}

	@Named("klachtenIntermenstrueelBloedverlies")
	static boolean klachtenIntermenstrueelBloedverlies(List<String> list)
	{
		return list.contains("intermenstrueel bloedverlies");
	}

	@Named("klachtenPostmenopauzaalBloedverlies")
	static boolean klachtenPostmenopauzaalBloedverlies(List<String> list)
	{
		return list.contains("postmenopauzaal bloedverlies");
	}

	@Named("klachtenAndersNamelijk")
	static boolean klachtenAndersNamelijk(List<String> list)
	{
		return list.contains("anders, namelijk...");
	}

	@Named("menstruatieNormaal")
	static boolean menstruatieNormaal(String string)
	{
		return "normaal".equalsIgnoreCase(getTrimmedLowercaseAnswer(string));
	}

	@Named("menstruatieGeenMenstruatie")
	static boolean menstruatieGeenMenstruatie(String string)
	{
		return "geen menstruatie".equalsIgnoreCase(getTrimmedLowercaseAnswer(string));
	}

	@Named("menstruatieMenopauze")
	static boolean menstruatieMenopauze(String string)
	{
		return "menopauze".equalsIgnoreCase(getTrimmedLowercaseAnswer(string));
	}

	@Named("menstruatiePostmenopauze")
	static boolean menstruatiePostmenopauze(String string)
	{
		return "postmenopauze (>1 jaar geen menstruatie)".equalsIgnoreCase(getTrimmedLowercaseAnswer(string));
	}

	@Named("anticonceptieGeen")
	static boolean anticonceptieGeen(String string)
	{
		return "geen".equalsIgnoreCase(getTrimmedLowercaseAnswer(string));
	}

	@Named("anticonceptiePil")
	static boolean anticonceptiePil(String string)
	{
		return "pil (hormonale therapie)".equalsIgnoreCase(getTrimmedLowercaseAnswer(string));
	}

	@Named("anticonceptieIudKoper")
	static boolean anticonceptieIudKoper(String string)
	{
		return "iud koper".equalsIgnoreCase(getTrimmedLowercaseAnswer(string));
	}

	@Named("anticonceptieIudMirena")
	static boolean anticonceptieIudMirena(String string)
	{
		return "hormoonhoudend spiraal".equalsIgnoreCase(getTrimmedLowercaseAnswer(string));
	}

	@Named("anticonceptieAnders")
	static boolean anticonceptieAnders(String string)
	{
		return "anders".equalsIgnoreCase(getTrimmedLowercaseAnswer(string));
	}

	@Named("gebruikHormonenJaVanwegeOvergangsklachten")
	static boolean gebruikHormonenJaVanwegeOvergangsklachten(List<String> list)
	{
		return list.contains("vanwege overgangsklachten");
	}

	@Named("gebruikHormonenJaVanwegeBorstkanker")
	static boolean gebruikHormonenJaVanwegeBorstkanker(List<String> list)
	{
		return list.contains("vanwege borstkanker");
	}

	@Named("gebruikHormonenJaVanwege")
	static boolean gebruikHormonenJaVanwege(List<String> list)
	{
		return list.contains("vanwege...");
	}

	@Named("aspectCervixNormaal")
	static boolean aspectCervixNormaal(String string)
	{
		return "normaal".equalsIgnoreCase(getTrimmedLowercaseAnswer(string));
	}

	@Named("aspectCervixAbnormaalOfVerdachtePortio")
	static boolean aspectCervixAbnormaalOfVerdachtePortio(String string)
	{
		return "abnormaal of verdachte portio".equalsIgnoreCase(getTrimmedLowercaseAnswer(string));
	}

	@Named("getTrimmedAnswer")
	static String getTrimmedAnswer(String string)
	{
		return StringUtils.trim(string);
	}

	@Named("getTrimmedLowercaseAnswer")
	static String getTrimmedLowercaseAnswer(String string)
	{
		return string != null
			? string.trim().toLowerCase()
			: string;
	}

}
