package nl.rivm.screenit.model.enums;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.io.File;
import java.util.Calendar;

import lombok.Getter;

public enum FileStoreLocation
{

	CERVIX_BETALING_SEPA(File.separator + "cervix" + File.separator + "betaling" + File.separator + "{id}" + File.separator + "sepa", true),
	CERVIX_BETALING_PDF(File.separator + "cervix" + File.separator + "betaling" + File.separator + "{id}" + File.separator + "pdf", true),

	CERVIX_MERGED_BRIEVEN(File.separator + "cervix" + File.separator + "brieven" + File.separator + "mergedbrieven" + File.separator, true),

	CERVIX_BRIEF_TEMPLATES(File.separator + "cervix" + File.separator + "brieven" + File.separator + "templates" + File.separator, true),

	CERVIX_AFMELDING(
		File.separator + "cervix" + File.separator + "client" + File.separator + "{id}" + File.separator + "afmelding" + File.separator + "handtekeningbrief" + File.separator,
		false,
		true),

	CERVIX_HERAANMELDING(
		File.separator + "cervix" + File.separator + "client" + File.separator + "{id}" + File.separator + "heraanmelding" + File.separator + "handtekeningbrief" + File.separator,
		false,
		true),

	CERVIX_BULK_HUISARTSEN(File.separator + "cervix" + File.separator + "huisartsen" + File.separator + "bulk" + File.separator, false),

	CERVIX_ZAS_UITNODIGINGEN_INPAKCENTRUM(File.separator + "cervix" + File.separator + "zasuitnodigingen" + File.separator + "mergedbrieven" + File.separator, true),

	COLON_UITNODIGINGEN_INPAKCENTRUM(File.separator + "colon" + File.separator + "colonuitnodigingen" + File.separator + "mergedbrieven" + File.separator, true),

	COLON_MERGED_BRIEVEN(File.separator + "colon" + File.separator + "brieven" + File.separator + "mergedbrieven" + File.separator, true),

	COLON_BRIEF_TEMPLATES(File.separator + "colon" + File.separator + "brieven" + File.separator + "templates" + File.separator, true),

	COLON_AFMELDING(
		File.separator + "colon" + File.separator + "client" + File.separator + "{id}" + File.separator + "afmelding" + File.separator + "handtekeningbrief",
		false,
		true),

	COLON_HERAANMELDING(
		File.separator + "colon" + File.separator + "client" + File.separator + "{id}" + File.separator + "heraanmelding" + File.separator + "handtekeningbrief",
		false,
		true),

	COLON_OVEREENKOMST(File.separator + "colon" + File.separator + "overeenkomst" + File.separator, true),

	COLON_RETOURZENDING(File.separator + "colon" + File.separator + "retourzending" + File.separator, true),

	COLON_INTERVALCARCINOOM(File.separator + "colon" + File.separator + "koppelresultatenkankerregistratie" + File.separator, true),

	MAMMA_VISITATIE_BIJLAGE(
		File.separator + "mamma" + File.separator + "kwaliteitscontrole" + File.separator + "visitatie" + File.separator + "{id}" + File.separator,
		false),

	MAMMA_STANDPLAATS_LOCATIE_BIJLAGE(
		File.separator + "mamma" + File.separator + "standplaats" + File.separator + "{id}" + File.separator + "locatiebijlage" + File.separator,
		true),

	MAMMA_PALGA_CSV_EXPORT(
		File.separator + "mamma" + File.separator + "palga" + File.separator + "export" + File.separator,
		false),

	MAMMA_PALGA_CSV_IMPORT(
		File.separator + "mamma" + File.separator + "palga" + File.separator + "import" + File.separator,
		false),

	PROJECT_BRIEF_TEMPLATES(
		File.separator + "algemeen" + File.separator + "project" + File.separator + "{id}" + File.separator + "brieven" + File.separator + "templates" + File.separator,
		true,
		true),

	PROJECT_MERGED_BRIEVEN(
		File.separator + "algemeen" + File.separator + "project" + File.separator + "{id}" + File.separator + "brieven" + File.separator + "mergedbrieven" + File.separator,
		true,
		true),

	PROJECT_IMPORT(File.separator + "algemeen" + File.separator + "project" + File.separator + "{id}" + File.separator + "import" + File.separator, false, true),

	PROJECT_BESTAND(File.separator + "algemeen" + File.separator + "project" + File.separator + "{id}" + File.separator + "bestanden" + File.separator, true, true),

	PROJECT_INACTIVEREN(File.separator + "algemeen" + File.separator + "project" + File.separator + "{id}" + File.separator + "inactiveren" + File.separator, false, true),

	BEZWAAR(File.separator + "algemeen" + File.separator + "client" + File.separator + "{id}" + File.separator + "bezwaar" + File.separator, false, true),

	OVERDRACHT_PERSOONSGEVENS(
		File.separator + "algemeen" + File.separator + "client" + File.separator + "{id}" + File.separator + "overdrachtpersoonsgegevens" + File.separator,
		false,
		true),

	ALGEMEEN_MERGED_BRIEVEN(File.separator + "algemeen" + File.separator + "brieven" + File.separator + "mergedbrieven" + File.separator, true),

	INSTELLING_MERGED_BRIEVEN(
		File.separator + "algemeen" + File.separator + "instelling" + File.separator + "{id}" + File.separator + "mergedbrieven" + File.separator,
		true,
		true),

	BRIEF_TEMPLATES(File.separator + "algemeen" + File.separator + "brieven" + File.separator + "templates" + File.separator, true),

	SCREENINGORGANISATIE_AFBEELDINGEN(
		File.separator + "algemeen" + File.separator + "instelling" + File.separator + "{id}" + File.separator + "afbeeldingen" + File.separator,
		false,
		true),

	GEBRUIKER_AFBEELDINGEN(
		File.separator + "algemeen" + File.separator + "gebruiker" + File.separator + "{id}" + File.separator + "afbeeldingen" + File.separator,
		false,
		true),

	INSTELLING_DOCUMENTEN(File.separator + "algemeen" + File.separator + "instelling" + File.separator + "{id}" + File.separator + "documenten" + File.separator, false, true),

	CLIENT_DOCUMENTEN(File.separator + "algemeen" + File.separator + "client" + File.separator + "{id}" + File.separator + "documenten" + File.separator, false, true),

	VRAGENLIJSTEN_TEMPLATES(File.separator + "algemeen" + File.separator + "vragenlijst" + File.separator + "templates" + File.separator, true),

	MAMMA_MERGED_BRIEVEN(File.separator + "mamma" + File.separator + "brieven" + File.separator + "mergedbrieven" + File.separator, true),

	MAMMA_AFMELDING(
		File.separator + "mamma" + File.separator + "client" + File.separator + "{id}" + File.separator + "afmelding" + File.separator + "handtekeningbrief" + File.separator,
		false,
		true),

	MAMMA_HERAANMELDING(
		File.separator + "mamma" + File.separator + "client" + File.separator + "{id}" + File.separator + "heraanmelding" + File.separator + "handtekeningbrief" + File.separator,
		false,
		true),

	MAMMA_VERSLAG(
		File.separator + "mamma" + File.separator + "client" + File.separator + "{id}" + File.separator + "beoordeling" + File.separator + "verslag" + File.separator,
		false,
		true),

	MAMMA_KANSBEREKENING(File.separator + "mamma" + File.separator + "kansberekening", false, false),

	MAMMA_VERZAMELDE_ONDERZOEK_DATA(File.separator + "mamma" + File.separator + "verzamelde_onderzoek_data", false, false),

	CERVIX_UITSLAG_VERWIJDEREN_CLIENT_BRIEF(
		File.separator + "cervix" + File.separator + "client" + File.separator + "{id}" + File.separator + "documenten" + File.separator,
		true,
		true),

	COLON_UITSLAG_VERWIJDEREN_CLIENT_BRIEF(
		File.separator + "colon" + File.separator + "client" + File.separator + "{id}" + File.separator + "documenten" + File.separator,
		false,
		true),

	MAMMA_UPLOAD_BEELDEN(File.separator + "mamma" + File.separator + "uploadBeelden", false, false),

	REGRESSIETEST(File.separator + "test" + File.separator, false, false);

	private final String path;

	@Getter
	private final boolean saveFileWithId;

	private final boolean daysDirectories;

	FileStoreLocation(String path, boolean daysDirectories)
	{
		this.path = path;
		this.daysDirectories = daysDirectories;
		this.saveFileWithId = false;
	}

	FileStoreLocation(String path, boolean daysDirectories, boolean saveFileWithId)
	{
		this.path = path;
		this.daysDirectories = daysDirectories;
		this.saveFileWithId = saveFileWithId;
	}

	public String getPath(Long id)
	{
		return generatePath(path, id);
	}

	public String getPath()
	{
		return generatePath(path);
	}

	private String generatePath(String path)
	{
		return generatePath(path, null);
	}

	private String generatePath(String path, Long id)
	{
		StringBuilder directory = new StringBuilder();

		directory.append(path);

		if (daysDirectories)
		{
			Calendar cal = Calendar.getInstance();
			directory.append(File.separator);
			directory.append(cal.get(Calendar.YEAR));
			directory.append(File.separator);
			directory.append(cal.get(Calendar.MONTH) + 1);
			directory.append(File.separator);
			directory.append(cal.get(Calendar.DAY_OF_MONTH));
			directory.append(File.separator);
		}

		String rightPath = directory.toString();
		if (id != null)
		{
			return rightPath.replace("{id}", id.toString());
		}
		return rightPath;
	}

	public static FileStoreLocation getAfmelding(Bevolkingsonderzoek bevolkingsonderzoek)
	{
		switch (bevolkingsonderzoek)
		{
		case COLON:
			return COLON_AFMELDING;
		case CERVIX:
			return CERVIX_AFMELDING;
		case MAMMA:
			return MAMMA_AFMELDING;
		default:
			return null;
		}
	}

	public static FileStoreLocation getHeraanmelding(Bevolkingsonderzoek bevolkingsonderzoek)
	{
		switch (bevolkingsonderzoek)
		{
		case COLON:
			return COLON_HERAANMELDING;
		case CERVIX:
			return CERVIX_HERAANMELDING;
		case MAMMA:
			return MAMMA_HERAANMELDING;
		default:
			return null;
		}
	}
}
