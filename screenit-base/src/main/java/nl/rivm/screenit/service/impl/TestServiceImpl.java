package nl.rivm.screenit.service.impl;

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
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Scanner;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.dao.ClientDao;
import nl.rivm.screenit.dao.CoordinatenDao;
import nl.rivm.screenit.dao.GbaBaseDao;
import nl.rivm.screenit.model.BagAdres;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientBrief;
import nl.rivm.screenit.model.ClientContact;
import nl.rivm.screenit.model.ClientContactActie;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.MergedBrieven;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.GbaStatus;
import nl.rivm.screenit.model.gba.Land;
import nl.rivm.screenit.model.gba.Nationaliteit;
import nl.rivm.screenit.model.project.ProjectBrief;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.rivm.screenit.model.project.ProjectGroep;
import nl.rivm.screenit.model.project.ProjectInactiveerDocument;
import nl.rivm.screenit.service.DossierFactory;
import nl.rivm.screenit.service.TestService;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.service.mamma.MammaBaseKansberekeningService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.TestBsnGenerator;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Geslacht;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.NaamGebruik;
import nl.topicuszorg.util.postcode.PostcodeFormatter;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Restrictions;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.base.Charsets;

import au.com.bytecode.opencsv.CSVParser;

@Slf4j
@Service
@Transactional(propagation = Propagation.REQUIRED)
public class TestServiceImpl implements TestService
{
	@Autowired
	private HibernateService hibernateService;

	@Autowired(required = false)
	private MammaBaseKansberekeningService baseKansberekeningService;

	@Autowired
	private ClientDao clientDao;

	@Autowired
	private CoordinatenDao coordinatenDao;

	@Autowired
	private GbaBaseDao gbaDao;

	@Autowired
	private DossierFactory dossierFactory;

	@Autowired
	private UploadDocumentService uploadDocumentService;

	private static String getValue2(String value)
	{
		return correctLength(StringUtils.rightPad(value, 240, ' '), 240);
	}

	private static String getValue(String value)
	{
		return correctLength(StringUtils.rightPad(value, 254, ' '), 254);
	}

	private static String correctLength(String value, int length)
	{
		if (value != null)
		{
			int diff = value.getBytes(Charsets.UTF_8).length - length;
			if (diff > 0)
			{
				value = value.substring(0, length - diff);
			}
			return value;
		}
		else
		{
			return StringUtils.rightPad("", length, ' ');
		}
	}

	@Override
	public GbaPersoon maakPersoon(LocalDate geboorteDatum)
	{
		GbaPersoon persoon = new GbaPersoon();
		persoon.setGeslacht(Geslacht.VROUW);
		persoon.setAchternaam("Lange");
		persoon.setTussenvoegsel("de");
		persoon.setVoornaam("Jenissi Gratia Bouwena Engeltje Herberdina Elisabeth Christina Daniëlle Willemijntje Johanna Maria Morgenstar");
		persoon.setBsn(TestBsnGenerator.getValideBsn());
		persoon.setGeboortedatum(DateUtil.toUtilDate(geboorteDatum));
		BagAdres adres = new BagAdres();
		adres.setPostcode("1234AA");
		adres.setPlaats("Hamsterdam");
		adres.setStraat("Hamsterdamseweg");
		adres.setHuisnummer(3);
		adres.setGbaGemeente(hibernateService.loadAll(Gemeente.class).get(0));
		persoon.setGbaAdres(adres);

		return persoon;
	}

	@Override
	public Client maakClient(GbaPersoon filter)
	{
		Client client = geefClient(filter.getBsn(), filter.getGeboortedatum(), filter.getOverlijdensdatum(), filter.getGeslacht());
		geefAdres(client, filter.getGbaAdres());
		hibernateService.saveOrUpdate(client.getPersoon());
		return client;
	}

	@Override
	public void createGbaFile(GbaPersoon persoon, InputStream vo107template, OutputStream vo107)
	{
		OutputStreamWriter vo107Bestand = null;

		try (Scanner scanner = new Scanner(vo107template, "UTF-8"))
		{
			vo107Bestand = new OutputStreamWriter(vo107);
			String vo107TemplateString = scanner.useDelimiter("\\A").next().substring(1);

			int arecordNr = 1;
			String vo107Regel = vo107TemplateString;
			Client client = geefClient(persoon.getBsn(), persoon.getGeboortedatum(), persoon.getOverlijdensdatum(), persoon.getGeslacht());
			GbaPersoon gbaPersoon = client.getPersoon();
			String anummer = gbaPersoon.getAnummer();
			BagAdres gbaAdres = gbaPersoon.getGbaAdres();
			String postcode = PostcodeFormatter.formatPostcode(gbaAdres.getPostcode(), false);
			if (postcode == null)
			{
				postcode = "1234AA";
			}
			vo107Regel = vo107Regel.replaceAll("100000000110", "1" + StringUtils.leftPad("" + arecordNr++, 11, '0'));

			vo107Regel = vo107Regel.replaceAll("<Anummer>", anummer);
			String gbaBerichtType = "Ag31";

			vo107Regel = vo107Regel.replace("Ag31", gbaBerichtType);

			String bsn = gbaPersoon.getBsn();
			vo107Regel = vo107Regel.replaceAll("<BSN>", StringUtils.rightPad(bsn, 9, ' '));
			vo107Regel = vo107Regel.replaceAll("<Geslachtsaanduiding>", gbaPersoon.getGeslacht().getMnem());
			vo107Regel = vo107Regel.replaceAll("<Geboortedatum>", StringUtils.rightPad(getGbaDatum("yyyyMMdd", gbaPersoon.getGeboortedatum()), 8, ' '));
			vo107Regel = vo107Regel.replaceAll("<Geboorteplaats>", getValue(gbaPersoon.getGeboorteplaats()));
			vo107Regel = vo107Regel.replaceAll("<Voornamen>", getValue(gbaPersoon.getVoornaam()));
			vo107Regel = vo107Regel.replaceAll("<Tussenvoegsel>", getValue(gbaPersoon.getTussenvoegsel()));
			vo107Regel = vo107Regel.replaceAll("<Geslachtsnaam>", getValue(gbaPersoon.getAchternaam()));

			vo107Regel = vo107Regel.replaceAll("<DatumOverlijden>", StringUtils.rightPad(getGbaDatum("yyyyMMdd", gbaPersoon.getOverlijdensdatum()), 8, ' '));

			if (CollectionUtils.isNotEmpty(gbaPersoon.getGbaNationaliteiten()))
			{
				vo107Regel = vo107Regel.replaceAll("<Nationaliteit>", getValue2(gbaPersoon.getGbaNationaliteiten().get(0).getNaam()));
				vo107Regel = vo107Regel.replaceAll("<Nationaliteitcode>", StringUtils.leftPad(gbaPersoon.getGbaNationaliteiten().get(0).getCode(), 4, '0'));
			}
			else
			{
				vo107Regel = vo107Regel.replaceAll("<Nationaliteit>", getValue2(""));
				vo107Regel = vo107Regel.replaceAll("<Nationaliteitcode>", StringUtils.leftPad("", 4, '0'));
			}

			if (gbaPersoon.getGbaGeboorteLand() != null)
			{
				vo107Regel = vo107Regel.replaceAll("<Geboorteland>", getValue2(gbaPersoon.getGbaGeboorteLand().getNaam()));
				vo107Regel = vo107Regel.replaceAll("<Geboortlandcode>", StringUtils.leftPad(gbaPersoon.getGbaGeboorteLand().getCode(), 4, '0'));
			}
			else
			{
				vo107Regel = vo107Regel.replaceAll("<Geboorteland>", getValue2(""));
				vo107Regel = vo107Regel.replaceAll("<Geboortlandcode>", StringUtils.leftPad("", 4, '0'));
			}
			vo107Regel = vo107Regel.replaceAll("<Straatnaam>", getValue(gbaAdres.getStraat()));

			vo107Regel = vo107Regel.replaceAll("<Huisnummer>", StringUtils.rightPad(gbaAdres.getHuisnummer() + "", 254, ' '));

			vo107Regel = vo107Regel.replaceAll("<Huisletter>", getValue(gbaAdres.getHuisletter()));

			vo107Regel = vo107Regel.replaceAll("<Huisnummertoevoeging>", getValue(gbaAdres.getHuisnummerToevoeging()));

			vo107Regel = vo107Regel.replaceAll("<Aanduiding bij huisnummer>", getValue(gbaAdres.getHuisnummerAanduiding()));

			vo107Regel = vo107Regel.replaceAll("<Gemeentedeel>", getValue(gbaAdres.getGemeentedeel()));

			vo107Regel = vo107Regel.replaceAll("<Postcode>", StringUtils.leftPad(postcode, 6, ' '));

			vo107Regel = vo107Regel.replaceAll("<Woonplaatsnaam>", getValue(gbaAdres.getPlaats()));

			vo107Regel = vo107Regel.replaceAll("<Gemeente van inschrijving>", getValue2(gbaAdres.getGbaGemeente().getNaam()));
			vo107Regel = vo107Regel.replaceAll("<Gemeentecode>", StringUtils.leftPad(gbaAdres.getGbaGemeente().getCode(), 4, '0'));

			vo107Bestand.append(vo107Regel);
			vo107Bestand.flush();
			scanner.close();
		}
		catch (Exception e)
		{
			LOG.error(e.getMessage(), e);
		}
	}

	@Override
	public Client getClientByBsn(String bsn)
	{
		return clientDao.getClientByBsn(bsn);
	}

	private Client geefClient(String bsn, Date geboortedatum, Date overlijdensDatum, Geslacht geslacht)
	{
		Client client = clientDao.getClientByBsn(bsn);
		if (client == null)
		{
			client = new Client();
			GbaPersoon persoon = new GbaPersoon();
			client.setPersoon(persoon);
			persoon.setGbaGeboorteLand(getGbaLand());
			persoon.setGbaNationaliteiten(List.of(getGbaNationaliteit()));
			persoon.setGeboorteplaats("Deventer");
			persoon.setNaamGebruik(NaamGebruik.EIGEN);
			switch (geslacht)
			{
			case MAN:
				persoon.setVoornaam("J\u00F6hn");
				persoon.setAchternaam("Doe\u00E7-" + bsn);
				break;
			case VROUW:
				persoon.setVoornaam("J\u00FCne");
				persoon.setAchternaam("Doe\u011F-" + bsn);
				break;
			case ONBEKEND:
				persoon.setVoornaam("J\u00E6mie");
				persoon.setAchternaam("Doe\uA7A1-" + bsn);
				break;
			case NIET_GESPECIFICEERD:
				throw new UnsupportedOperationException(geslacht + " Wordt niet gebruikt");
			default:
				throw new IllegalStateException("Unexpected value: " + geslacht);
			}
			persoon.setGeslacht(geslacht);
			persoon.setTussenvoegsel("de");
			persoon.setBsn(bsn);
			persoon.setOverlijdensdatum(overlijdensDatum);
			zetGeboortedatum(geboortedatum, persoon);
			persoon.setPatient(client);
			client.setGbaStatus(GbaStatus.INDICATIE_AANWEZIG);
			hibernateService.saveOrUpdate(persoon);
			hibernateService.saveOrUpdate(client);

			dossierFactory.maakDossiers(client);

			if (client.getMammaDossier() != null)
			{
				baseKansberekeningService.maakDossierEvent(client.getMammaDossier());
			}
		}

		GbaPersoon persoon = client.getPersoon();
		persoon.setOverlijdensdatum(overlijdensDatum);
		hibernateService.saveOrUpdate(persoon);

		return client;
	}

	public static void zetGeboortedatum(Date geboortedatum, GbaPersoon persoon)
	{
		if (geboortedatum == null)
		{
			try
			{
				persoon.setGeboortedatum(new SimpleDateFormat("dd-MM-yyyy").parse("01-01-1950"));
			}
			catch (ParseException e)
			{
				LOG.error(e.getMessage(), e);
			}
		}
		else
		{
			persoon.setGeboortedatum(geboortedatum);
		}
	}

	private BagAdres geefAdres(Client client, BagAdres bagAdres)
	{
		BagAdres gbaAdres = client.getPersoon().getGbaAdres();
		Gemeente gemeente = bagAdres.getGbaGemeente();
		if (gbaAdres == null)
		{
			gbaAdres = new BagAdres();
			hibernateService.saveOrUpdate(client.getPersoon());
			client.getPersoon().setGbaAdres(gbaAdres);
			hibernateService.saveOrUpdate(gbaAdres);
			if (gemeente == null)
			{
				gemeente = getGemeenteMetScreeningOrganisatie();
			}
			gbaAdres.setGbaGemeente(gemeente);
			gbaAdres.setPlaats(gemeente.getNaam());
			gbaAdres.setStraat("Teststraat");
			gbaAdres.setHuisnummer(9);
			String postcode = "1111XX";
			if (bagAdres.getPostcode() != null)
			{
				postcode = bagAdres.getPostcode();
			}
			gbaAdres.setPostcode(postcode);
			hibernateService.saveOrUpdate(gbaAdres);
		}
		else if (bagAdres.getGbaGemeente() != null)
		{
			gbaAdres.setGbaGemeente(bagAdres.getGbaGemeente());
			gbaAdres.setPlaats(bagAdres.getGbaGemeente().getNaam());
			hibernateService.saveOrUpdate(gbaAdres);
		}
		return gbaAdres;
	}

	@Override
	public Gemeente getGemeenteMetScreeningOrganisatie()
	{
		return (Gemeente) hibernateService.getHibernateSession().createCriteria(Gemeente.class).add(Restrictions.isNotNull("screeningOrganisatie"))
			.addOrder(Order.asc("naam")).list().get(0);
	}

	@Override
	public Land getGbaLand()
	{
		List<Land> landen = hibernateService.getHibernateSession().createCriteria(Land.class).add(Restrictions.eq("code", "6030")).list();
		if (landen.isEmpty())
		{
			Land nederland = new Land();
			nederland.setNaam("Nederland");
			nederland.setCode("6030");
			hibernateService.saveOrUpdate(nederland);
			return nederland;
		}
		return landen.get(0);
	}

	private Nationaliteit getGbaNationaliteit()
	{
		List<Nationaliteit> nationaliteiten = hibernateService.getHibernateSession().createCriteria(Nationaliteit.class).add(Restrictions.eq("code", "0001")).list();
		if (nationaliteiten.isEmpty())
		{
			Nationaliteit nat = new Nationaliteit();
			nat.setNaam("Nederlandse");
			nat.setCode("0001");
			hibernateService.saveOrUpdate(nat);
			return nat;
		}
		return nationaliteiten.get(0);
	}

	private String getGbaDatum(String format, Date datum)
	{
		if (datum != null)
		{
			return new SimpleDateFormat(format).format(datum);
		}
		return "";
	}

	@Override
	public void importClientenViaCsv(File file, ImportBvoViaCsv importBvoViaCsv) throws IOException, ParseException
	{
		List<Client> clienten = new ArrayList<>();
		Scanner scanner = new Scanner(new FileInputStream(file), com.google.common.base.Charsets.ISO_8859_1.name());

		Map<String, Integer> headersIndex = new HashMap<>();
		try
		{
			CSVParser csvParser = new CSVParser(';');

			boolean first = true;
			while (scanner.hasNextLine())
			{
				String line = scanner.nextLine();

				String[] columns = csvParser.parseLine(line);

				if (first)
				{
					int i = 0;
					for (String columnHeader : columns)
					{
						headersIndex.put(columnHeader, i++);
					}
					first = false;
				}
				else
				{

					Client client = null;

					String bsn = columns[headersIndex.get("BSN")];
					client = clientDao.getClientByBsn(bsn);

					if (client == null)
					{
						client = new Client();
						client.setGbaStatus(GbaStatus.INDICATIE_AANWEZIG);
					}

					GbaPersoon persoon = new GbaPersoon();
					client.setPersoon(persoon);
					persoon.setPatient(client);

					persoon.setBsn(bsn);
					persoon.setAnummer(columns[headersIndex.get("Anummer")]);
					persoon.setGeslacht(Geslacht.getGeslacht(columns[headersIndex.get("Geslacht")].toUpperCase()));
					persoon.setGeboortedatum(new SimpleDateFormat("yyyyMMdd").parse(columns[headersIndex.get("GeboorteDatum")]));
					persoon.setGeboorteplaats(columns[headersIndex.get("Geboorteplaats")]);
					persoon.setVoorletters(columns[headersIndex.get("Voorletters")]);

					persoon.setTussenvoegsel(columns[headersIndex.get("VoorvGeslachtsnaam")]);
					persoon.setAchternaam(columns[headersIndex.get("Geslachtsnaam")]);
					persoon.setPartnerTussenvoegsel(columns[headersIndex.get("VoorvNaamEchtgenoot")]);
					persoon.setPartnerAchternaam(columns[headersIndex.get("GeslachtsnaamEchtgenoot")]);
					if (org.apache.commons.lang.StringUtils.isNotBlank(columns[headersIndex.get("DatumOverlijden")]))
					{
						persoon.setOverlijdensdatum(new SimpleDateFormat("yyyyMMdd").parse(columns[headersIndex.get("DatumOverlijden")]));
					}

					Nationaliteit nationaliteit = gbaDao.getStamtabelByCode(Nationaliteit.class,
						org.apache.commons.lang.StringUtils.leftPad(columns[headersIndex.get("CodeNationaliteit")], 4, "0"));

					client.getPersoon().getGbaNationaliteiten().add(nationaliteit);

					nl.rivm.screenit.model.gba.Land geboorteLand = gbaDao.getStamtabelByCode(nl.rivm.screenit.model.gba.Land.class, columns[headersIndex.get("CodeGeboorteland")]);
					client.getPersoon().setGbaGeboorteLand(geboorteLand);

					BagAdres adres = new BagAdres();
					adres.setStraat(columns[headersIndex.get("Straat")]);

					if (org.apache.commons.lang.StringUtils.isNotBlank(columns[headersIndex.get("HuisNummer")]))
					{
						adres.setHuisnummer(Integer.parseInt(columns[headersIndex.get("HuisNummer")]));
					}

					adres.setHuisletter(columns[headersIndex.get("HuisLetter")]);
					adres.setHuisnummerToevoeging(columns[headersIndex.get("HuisnrToevoeging")]);
					adres.setHuisnummerAanduiding(columns[headersIndex.get("HuisAanduiding")]);
					adres.setGemeentedeel(columns[headersIndex.get("Gemeentedeel")]);

					String postcode = columns[headersIndex.get("Postcode")];
					postcode = postcode.toUpperCase().replaceAll(" ", "");

					if (org.apache.commons.lang.StringUtils.isBlank(postcode) )
					{
						LOG.error("Skipping patient, invalide postcode");
						continue;
					}

					adres.setPostcode(postcode);
					adres.setPlaats(columns[headersIndex.get("Woonplaats")]);
					String gemeenteCode = columns[headersIndex.get("Gemeentecode")];

					Gemeente gemeente = (Gemeente) hibernateService.getHibernateSession().createCriteria(Gemeente.class).add(Restrictions.eq("code", gemeenteCode)).uniqueResult();
					if (gemeente == null)
					{
						List<ScreeningOrganisatie> allSOs = hibernateService.loadAll(ScreeningOrganisatie.class);
						if (allSOs.size() > 0)
						{
							gemeente = new Gemeente();
							gemeente.setCode(gemeenteCode);
							gemeente.setNaam("Gemeente " + gemeenteCode);
							gemeente.setScreeningOrganisatie(allSOs.get(0));
							hibernateService.saveOrUpdate(gemeente);
						}
					}
					adres.setGbaGemeente(gemeente);
					adres.setPostcodeCoordinaten(coordinatenDao.getCoordinaten(adres));
					persoon.setGbaAdres(adres);

					if (org.apache.commons.lang.StringUtils.isNotBlank(columns[headersIndex.get("DatumVertrekUitNederland")]))
					{
						persoon.setDatumVertrokkenUitNederland(new SimpleDateFormat("yyyyMMdd").parse(columns[headersIndex.get("DatumVertrekUitNederland")]));
					}

					persoon.setTelefoonnummer1(columns[headersIndex.get("TelNrPrive")]);
					persoon.setTelefoonnummer2(columns[headersIndex.get("TelNrWerk")]);
					persoon.setEmailadres(columns[headersIndex.get("EmailAdres")]);

					clientDao.saveOrUpdateClient(client);

					importBvoViaCsv.importBvoViaCsv(headersIndex, columns, client);

					clientDao.saveOrUpdateClient(client);
					clienten.add(client);

					dossierFactory.maakDossiers(client);
					if (client.getMammaDossier() != null)
					{
						baseKansberekeningService.maakDossierEvent(client.getMammaDossier());
					}
				}

			}
		}
		finally
		{
			LOG.info("Stopped bij verwerking van client #" + (clienten.size() + 1));
			scanner.close();
		}
	}

	@Override
	public boolean verwijderClientContacten(Client client, Bevolkingsonderzoek... onderzoeken)
	{
		return verwijderClientContacten(client, Arrays.asList(onderzoeken));
	}

	@Override
	public boolean verwijderClientContacten(Client client, boolean isDossierVerwijderdDK, boolean isDossierVerwijderdBMHK, boolean isDossierVerwijderdBK)
	{
		List<Bevolkingsonderzoek> onderzoeken = new ArrayList<Bevolkingsonderzoek>();
		if (isDossierVerwijderdBMHK)
		{
			onderzoeken.add(Bevolkingsonderzoek.CERVIX);
		}
		if (isDossierVerwijderdDK)
		{
			onderzoeken.add(Bevolkingsonderzoek.COLON);
		}
		if (isDossierVerwijderdBK)
		{
			onderzoeken.add(Bevolkingsonderzoek.MAMMA);
		}
		return verwijderClientContacten(client, onderzoeken);
	}

	@Override
	public boolean verwijderClientContacten(Client client, List<Bevolkingsonderzoek> onderzoeken)
	{

		if (CollectionUtils.isNotEmpty(client.getContacten()))
		{
			List<ClientContact> verwijderdeContactenLijst = new ArrayList<>();
			for (ClientContact contact : client.getContacten())
			{
				boolean clientContactMagWeg = true;
				List<ClientContactActie> verwijderenContactActieLijst = new ArrayList<>();
				for (ClientContactActie actie : contact.getActies())
				{
					if (onderzoeken.equals(actie.getType().getBevolkingsonderzoeken()))
					{
						hibernateService.delete(actie);
						verwijderenContactActieLijst.add(actie);
					}
					else
					{
						clientContactMagWeg = false;
					}
				}
				contact.getActies().removeAll(verwijderenContactActieLijst);
				if (clientContactMagWeg)
				{
					hibernateService.delete(contact);
					verwijderdeContactenLijst.add(contact);
				}
			}
			client.getContacten().removeAll(verwijderdeContactenLijst);
		}
		return false;
	}

	@Override
	public void projectenVerwijderen(Client client)
	{
		for (ProjectClient pclient : client.getProjecten())
		{
			List<ProjectBrief> projectBrieven = pclient.getBrieven();
			projectBrieven.forEach(projectBrief ->
			{
				ClientBrief clientBrief = projectBrief.getBrief();
				if (clientBrief != null)
				{
					clientBrief.setProjectBrief(null);
				}
				MergedBrieven mergedBrieven = projectBrief.getMergedBrieven();
				if (mergedBrieven != null)
				{
					mergedBrieven.getBrieven().remove(projectBrief);
					hibernateService.saveOrUpdate(mergedBrieven);
				}
			});

			ProjectInactiveerDocument projectInactiveerDocument = pclient.getProjectInactiveerDocument();
			if (projectInactiveerDocument != null)
			{
				List<ProjectClient> projectClienten = projectInactiveerDocument.getProjectClienten();
				pclient.setProjectInactiveerDocument(null);
				if (projectClienten.isEmpty())
				{
					hibernateService.delete(projectInactiveerDocument);
					uploadDocumentService.delete(projectInactiveerDocument.getUploadDocument());
				}
				else
				{
					projectClienten.remove(pclient);
					hibernateService.saveOrUpdate(projectInactiveerDocument);
				}
			}
			hibernateService.deleteAll(projectBrieven);
			ProjectGroep groep = pclient.getGroep();
			groep.setPopulatie(groep.getPopulatie() - 1);
			groep.getClienten().remove(pclient);
			hibernateService.saveOrUpdateAll(groep);
			hibernateService.delete(pclient);
		}
		hibernateService.saveOrUpdate(client);
	}

}
