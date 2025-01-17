
package nl.rivm.screenit.main.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.io.FileReader;
import java.io.IOException;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.main.service.ProefBvoService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.model.AanvraagBriefStatus;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.AfmeldingType;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.colon.ColonAfmelding;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.enums.ColonAfmeldingReden;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.BaseAfmeldService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.util.DateUtil;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import au.com.bytecode.opencsv.CSVReader;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class ProefBvoServiceImpl implements ProefBvoService
{
	private static final String VOLDOET_NIET_AFGESPROKEN_FORMAT = "Voldoet niet aan het afgesproken formaat (&lt;anummer:{0-9}10&gt;;&lt;geboortedatum:{yyyyMMdd}&gt;).";

	private static final Logger LOG = LoggerFactory.getLogger(ProefBvoServiceImpl.class);

	@Autowired
	private ClientService clientService;

	@Autowired
	private BaseAfmeldService baseAfmeldService;

	@Autowired
	private LogService logService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	public List<String> afmelden(Account ingelogdeGebruiker, File fileAfmeldingBrief, String contentType, String fileName, File fileClientenBestand) throws IOException
	{
		logService.logGebeurtenis(LogGebeurtenis.PROOF_ONDERZOEK_AFMELDING_START, ingelogdeGebruiker, Bevolkingsonderzoek.COLON);

		List<String> meldingen = new ArrayList<>();
		int aantalAfgemeld = 0;
		int regelNr = 0;
		int totaalAantal = 0;
		long start = System.currentTimeMillis();

		try (CSVReader csvReader = new CSVReader(new FileReader(fileClientenBestand), ';'))
		{
			List<String[]> allLines = csvReader.readAll();
			totaalAantal = allLines.size();
			for (String[] line : allLines)
			{
				String meldingPrefix = "Regel #" + ++regelNr + " '" + StringUtils.join(line, ";") + "': ";
				try
				{
					Client client = getClient(line, meldingPrefix, meldingen);
					if (client != null)
					{
						boolean magDefinitiefAfmelden = true;
						ColonDossier colonDossier = client.getColonDossier();
						magDefinitiefAfmelden = Boolean.TRUE.equals(colonDossier.getAangemeld());
						if (magDefinitiefAfmelden)
						{
							ColonAfmelding afmelding = new ColonAfmelding();
							afmelding.setReden(ColonAfmeldingReden.PROEF_BEVOLKINGSONDERZOEK);
							afmelding.setType(AfmeldingType.DEFINITIEF);
							afmelding.setAfmeldingStatus(AanvraagBriefStatus.BRIEF);
							afmelding.setDossier(colonDossier);
							afmelding.setAfmeldDatum(currentDateSupplier.getDate());
							afmelding.setStatusAfmeldDatum(currentDateSupplier.getDate());

							UploadDocument document = new UploadDocument();
							document.setActief(Boolean.TRUE);
							document.setContentType(contentType);
							document.setFile(fileAfmeldingBrief);
							document.setNaam(fileName);
							afmelding.setHandtekeningDocumentAfmelding(document);

							baseAfmeldService.afmelden(afmelding.getDossier().getClient(), afmelding, ScreenitSession.get().getLoggedInInstellingGebruiker());
							logService.logGebeurtenis(LogGebeurtenis.AFMELDEN, ScreenitSession.get().getLoggedInAccount(), afmelding.getDossier().getClient(),
								"Type: " + afmelding.getType().name().toLowerCase(), afmelding.getBevolkingsonderzoek());

							aantalAfgemeld++;
						}
						else
						{
							meldingen.add(meldingPrefix + "Client met bsn " + client.getPersoon().getBsn() + " is al definitief afgemeld.");
						}
					}
				}
				catch (Exception e)
				{
					meldingen.add(meldingPrefix + VOLDOET_NIET_AFGESPROKEN_FORMAT + " Onbekende fout: " + e.getMessage());
					LOG.error("Proefbevolkings afmelden service afmelden zorgde voor exception", e);
				}
			}

		}
		catch (IOException e)
		{
			throw e;
		}

		meldingen.add(0, "Aantal gelezen regels: " + totaalAantal);
		meldingen.add(1, "Aantal verwerkte regels: " + aantalAfgemeld);
		long diff = System.currentTimeMillis() - start;
		LOG.debug("Totaal " + diff + " ms voor bestand clienten met proefbevolingsondezoek.");

		String melding = maakMeldingVoorLogEvent(meldingen);

		logService.logGebeurtenis(LogGebeurtenis.PROOF_ONDERZOEK_AFMELDING_AFGEROND, ingelogdeGebruiker, melding, Bevolkingsonderzoek.COLON);
		return meldingen;
	}

	@Override
	public List<String> heraanmelden(Account ingelogdeGebruiker, File fileAfmeldingBrief, String contentType, String fileName, File fileClientenBestand) throws IOException
	{
		logService.logGebeurtenis(LogGebeurtenis.PROOF_ONDERZOEK_HERAANMELDING_START, ingelogdeGebruiker, Bevolkingsonderzoek.COLON);
		long start = System.currentTimeMillis();

		ArrayList<String> meldingen = new ArrayList<>();
		int aantalHeraangemeld = 0;
		int regelNr = 0;
		int totaalAantal = 0;
		UploadDocument handtekeningDoc = null;
		try (CSVReader csvReader = new CSVReader(new FileReader(fileClientenBestand), ';'))
		{

			List<String[]> allLines = csvReader.readAll();
			totaalAantal = allLines.size();
			for (String[] line : allLines)
			{
				String meldingPrefix = "Regel #" + ++regelNr + " '" + StringUtils.join(line, ";") + "': ";
				try
				{
					Client client = getClient(line, meldingPrefix, meldingen);
					if (client != null)
					{
						ColonDossier dossier = client.getColonDossier();
						ColonAfmelding afmelding = dossier.getLaatsteAfmelding();
						if (afmelding != null && ColonAfmeldingReden.PROEF_BEVOLKINGSONDERZOEK.equals(afmelding.getReden())
							&& Boolean.FALSE.equals(client.getColonDossier().getAangemeld()))
						{

							afmelding.setStatusHeraanmeldDatum(currentDateSupplier.getDate());
							afmelding.setHeraanmeldStatus(AanvraagBriefStatus.BRIEF);
							afmelding.setClientWilNieuweUitnodiging(false);
							if (handtekeningDoc == null)
							{
								handtekeningDoc = new UploadDocument();
								handtekeningDoc.setActief(Boolean.TRUE);
								handtekeningDoc.setContentType(contentType);
								handtekeningDoc.setFile(fileAfmeldingBrief);
								handtekeningDoc.setNaam(fileName);
							}
							afmelding.setHandtekeningDocumentHeraanmelding(handtekeningDoc);
							baseAfmeldService.heraanmelden(afmelding, ingelogdeGebruiker);
							aantalHeraangemeld++;
						}
						else
						{
							meldingen.add(meldingPrefix
								+ "Client heeft niet de correcte status voor de heraanmelding. Zorg dat deze client een reden proefbevolking heeft of dossier is afgemeld.");
						}
					}
				}
				catch (Exception e)
				{
					meldingen.add(meldingPrefix + VOLDOET_NIET_AFGESPROKEN_FORMAT + " Onbekende fout: " + e.getMessage());
					LOG.error("Proefbevolkings afmelden service heraanmelden zorgde voor exception", e);
				}
			}
		}
		catch (IOException e)
		{
			throw e;
		}

		meldingen.add(0, "Aantal gelezen regels: " + totaalAantal);
		meldingen.add(1, "Aantal verwerkte regels: " + aantalHeraangemeld);
		long diff = System.currentTimeMillis() - start;
		LOG.debug("Totaal " + diff + " ms voor bestand clienten met proefbevolingsondezoek.");

		String melding = maakMeldingVoorLogEvent(meldingen);

		logService.logGebeurtenis(LogGebeurtenis.PROOF_ONDERZOEK_HERAANMELDING_AFGEROND, ingelogdeGebruiker, melding, Bevolkingsonderzoek.COLON);

		return meldingen;
	}

	private String maakMeldingVoorLogEvent(List<String> meldingen)
	{
		return StringUtils.join(meldingen, "<br>");
	}

	private Client getClient(String[] line, String meldingPrefix, List<String> meldingen)
	{
		Client result = null;
		try
		{
			String anummer = line[0];
			String geboortedatum = line[1];
			if (StringUtils.isNotBlank(anummer) && anummer.trim().length() == 10)
			{
				anummer = anummer.trim();
				Client client = clientService.getClientByAnummer(anummer);
				if (client != null)
				{
					if (StringUtils.isNotBlank(geboortedatum) && geboortedatum.trim().length() == 8)
					{
						if (DateUtil.isGeboortedatumGelijk(LocalDate.parse(geboortedatum, DateTimeFormatter.ofPattern(Constants.DATE_FORMAT_YYYYMMDD)), client))
						{
							result = client;
						}
						else
						{
							meldingen.add(meldingPrefix + "Geboortedatum in regel komt niet overeen met die van client met bsn " + client.getPersoon().getBsn() + ", geboortedatum "
								+ DateUtil.getGeboortedatum(client));
						}
					}
					else
					{
						meldingen.add(meldingPrefix + VOLDOET_NIET_AFGESPROKEN_FORMAT);
					}
				}
				else
				{
					meldingen.add(meldingPrefix + "Geen client gevonden voor anummer " + anummer);
				}
			}
			else
			{
				meldingen.add(meldingPrefix + VOLDOET_NIET_AFGESPROKEN_FORMAT);
			}
		}
		catch (Exception e)
		{
			LOG.error(e.getMessage(), e);
			meldingen.add(meldingPrefix + VOLDOET_NIET_AFGESPROKEN_FORMAT + " Onbekende fout: " + e.getMessage());
		}
		return result;
	}
}
