package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.nio.charset.StandardCharsets;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;
import java.util.Scanner;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.BagAdres;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContact;
import nl.rivm.screenit.model.ClientContactActie;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.Gemeente_;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.SingleTableHibernateObject_;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.GbaStatus;
import nl.rivm.screenit.repository.algemeen.GemeenteRepository;
import nl.rivm.screenit.repository.algemeen.InstellingRepository;
import nl.rivm.screenit.repository.cervix.BmhkLaboratoriumRepository;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.CoordinatenService;
import nl.rivm.screenit.service.DossierFactory;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.TestService;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.service.mamma.MammaBaseKansberekeningService;
import nl.rivm.screenit.specification.algemeen.GemeenteSpecification;
import nl.rivm.screenit.specification.algemeen.OrganisatieSpecification;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.TestBsnGenerator;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Geslacht;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.NaamGebruik;
import nl.topicuszorg.util.postcode.PostcodeFormatter;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.base.Charsets;

import au.com.bytecode.opencsv.CSVParser;

import static nl.rivm.screenit.specification.algemeen.GemeenteSpecification.heeftScreeningOrganisatie;

@Slf4j
@Service
public class TestServiceImpl implements TestService
{
	@Autowired
	private HibernateService hibernateService;

	@Autowired(required = false)
	private MammaBaseKansberekeningService baseKansberekeningService;

	@Autowired
	private ClientService clientService;

	@Autowired
	private CoordinatenService coordinatenService;

	@Autowired
	private DossierFactory dossierFactory;

	@Autowired
	private UploadDocumentService uploadDocumentService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private GemeenteRepository gemeenteRepository;

	@Autowired
	private InstellingRepository organisatieRepository;

	@Autowired
	private BmhkLaboratoriumRepository bmhkLaboratoriumRepository;

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
			var diff = value.getBytes(Charsets.UTF_8).length - length;
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
		var persoon = new GbaPersoon();
		persoon.setGeslacht(Geslacht.VROUW);
		persoon.setAchternaam("Lange");
		persoon.setTussenvoegsel("de");
		persoon.setVoornaam("Jenissi Gratia Bouwena Engeltje Herberdina Elisabeth Christina Daniëlle Willemijntje Johanna Maria Morgenstar");
		persoon.setBsn(TestBsnGenerator.getValideBsn());
		persoon.setGeboortedatum(DateUtil.toUtilDate(geboorteDatum));
		var adres = new BagAdres();
		adres.setPostcode("1234AA");
		adres.setPlaats("Hamsterdam");
		adres.setStraat("Hamsterdamseweg");
		adres.setHuisnummer(3);
		adres.setGbaGemeente(hibernateService.loadAll(Gemeente.class).get(0));
		persoon.setGbaAdres(adres);

		return persoon;
	}

	@Override
	@Transactional
	public Client maakClient(GbaPersoon filter)
	{
		var client = geefClient(filter.getBsn(), filter.getGeboortedatum(), filter.getOverlijdensdatum(), filter.getGeslacht());
		geefAdres(client, filter.getGbaAdres());

		hibernateService.saveOrUpdate(client.getPersoon());
		return client;
	}

	@Override
	@Transactional
	public void createGbaFile(GbaPersoon persoon, InputStream vo107template, OutputStream vo107)
	{
		OutputStreamWriter vo107Bestand;

		try (var scanner = new Scanner(vo107template, StandardCharsets.UTF_8))
		{
			vo107Bestand = new OutputStreamWriter(vo107);
			var vo107TemplateString = scanner.useDelimiter("\\A").next().substring(1);

			var arecordNr = 1;
			var vo107Regel = vo107TemplateString;
			var client = geefClient(persoon.getBsn(), persoon.getGeboortedatum(), persoon.getOverlijdensdatum(), persoon.getGeslacht());
			var gbaPersoon = client.getPersoon();
			var anummer = gbaPersoon.getAnummer();
			var gbaAdres = gbaPersoon.getGbaAdres();
			var postcode = PostcodeFormatter.formatPostcode(gbaAdres.getPostcode(), false);
			if (postcode == null)
			{
				postcode = "1234AA";
			}
			vo107Regel = vo107Regel.replaceAll("100000000110", "1" + StringUtils.leftPad("" + arecordNr++, 11, '0'));

			vo107Regel = vo107Regel.replaceAll("<Anummer>", anummer);
			var gbaBerichtType = "Ag31";

			vo107Regel = vo107Regel.replace("Ag31", gbaBerichtType);

			var bsn = gbaPersoon.getBsn();
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
		}
		catch (Exception e)
		{
			LOG.error(e.getMessage(), e);
		}
	}

	@Override
	public Client getClientByBsn(String bsn)
	{
		return clientService.getClientByBsn(bsn);
	}

	private Client geefClient(String bsn, Date geboortedatum, Date overlijdensDatum, Geslacht geslacht)
	{
		var client = clientService.getClientByBsn(bsn);
		if (client == null)
		{
			client = new Client();
			var persoon = new GbaPersoon();
			client.setPersoon(persoon);
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

		var persoon = client.getPersoon();
		persoon.setOverlijdensdatum(overlijdensDatum);
		hibernateService.saveOrUpdate(persoon);

		return client;
	}

	private static void zetGeboortedatum(Date geboortedatum, GbaPersoon persoon)
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

	private void geefAdres(Client client, BagAdres bagAdres)
	{
		var gbaAdres = client.getPersoon().getGbaAdres();
		var gemeente = bagAdres.getGbaGemeente();
		if (gbaAdres == null)
		{
			gbaAdres = new BagAdres();
			hibernateService.saveOrUpdate(client.getPersoon());
			client.getPersoon().setGbaAdres(gbaAdres);
			hibernateService.saveOrUpdate(gbaAdres);
			if (gemeente == null)
			{
				gemeente = getEersteGemeenteMetScreeningOrganisatie();
			}

			setDatumVertrokkenNederland(client, gemeente);
			gbaAdres.setGbaGemeente(gemeente);
			gbaAdres.setPlaats(gemeente.getNaam());
			gbaAdres.setStraat("Teststraat");
			gbaAdres.setHuisnummer(9);
			var postcode = "1111XX";
			if (bagAdres.getPostcode() != null)
			{
				postcode = bagAdres.getPostcode();
			}
			gbaAdres.setPostcode(postcode);
			hibernateService.saveOrUpdate(gbaAdres);
		}
		else if (bagAdres.getGbaGemeente() != null)
		{
			setDatumVertrokkenNederland(client, gemeente);
			gbaAdres.setGbaGemeente(bagAdres.getGbaGemeente());
			gbaAdres.setPlaats(bagAdres.getGbaGemeente().getNaam());

			hibernateService.saveOrUpdate(gbaAdres);
		}
	}

	private void setDatumVertrokkenNederland(Client client, Gemeente gemeente)
	{
		if (Objects.equals(gemeente.getCode(), Gemeente.RNI_CODE) && client.getPersoon().getDatumVertrokkenUitNederland() == null)
		{
			var datumGisteren = DateUtil.toUtilDate(currentDateSupplier.getLocalDateTime().minusDays(1));
			client.getPersoon().setDatumVertrokkenUitNederland(datumGisteren);
		}
		else if (!Objects.equals(gemeente.getCode(), Gemeente.RNI_CODE) && client.getPersoon().getDatumVertrokkenUitNederland() != null)
		{
			client.getPersoon().setDatumVertrokkenUitNederland(null);
		}
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
	@Transactional
	public void importClientenViaCsv(File file, ImportBvoViaCsv importBvoViaCsv) throws IOException, ParseException
	{
		var clienten = new ArrayList<Client>();
		try (var scanner = new Scanner(new FileInputStream(file), Charsets.ISO_8859_1))
		{

			var headersIndex = new HashMap<String, Integer>();
			try
			{
				var csvParser = new CSVParser(';');

				var first = true;
				while (scanner.hasNextLine())
				{
					var line = scanner.nextLine();

					var columns = csvParser.parseLine(line);

					if (first)
					{
						var i = 0;
						for (var columnHeader : columns)
						{
							headersIndex.put(columnHeader, i++);
						}
						first = false;
					}
					else
					{
						var bsn = columns[headersIndex.get("BSN")];
						var client = clientService.getClientByBsn(bsn);

						if (client == null)
						{
							client = new Client();
							client.setGbaStatus(GbaStatus.INDICATIE_AANWEZIG);
						}

						var persoon = new GbaPersoon();
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
						if (StringUtils.isNotBlank(columns[headersIndex.get("DatumOverlijden")]))
						{
							persoon.setOverlijdensdatum(new SimpleDateFormat("yyyyMMdd").parse(columns[headersIndex.get("DatumOverlijden")]));
						}

						var adres = new BagAdres();
						adres.setStraat(columns[headersIndex.get("Straat")]);

						if (StringUtils.isNotBlank(columns[headersIndex.get("HuisNummer")]))
						{
							adres.setHuisnummer(Integer.parseInt(columns[headersIndex.get("HuisNummer")]));
						}

						adres.setHuisletter(columns[headersIndex.get("HuisLetter")]);
						adres.setHuisnummerToevoeging(columns[headersIndex.get("HuisnrToevoeging")]);
						adres.setHuisnummerAanduiding(columns[headersIndex.get("HuisAanduiding")]);
						adres.setGemeentedeel(columns[headersIndex.get("Gemeentedeel")]);

						var postcode = columns[headersIndex.get("Postcode")];
						postcode = postcode.toUpperCase().replaceAll(" ", "");

						if (StringUtils.isBlank(postcode))
						{
							LOG.error("Skipping patient, invalide postcode");
							continue;
						}

						adres.setPostcode(postcode);
						adres.setPlaats(columns[headersIndex.get("Woonplaats")]);
						var gemeenteCode = columns[headersIndex.get("Gemeentecode")];
						var gemeente = getGemeenteByCode(gemeenteCode);

						if (gemeente == null)
						{
							var allSOs = hibernateService.loadAll(ScreeningOrganisatie.class);
							if (!allSOs.isEmpty())
							{
								gemeente = new Gemeente();
								gemeente.setCode(gemeenteCode);
								gemeente.setNaam("Gemeente " + gemeenteCode);
								gemeente.setScreeningOrganisatie(allSOs.get(0));
								hibernateService.saveOrUpdate(gemeente);
							}
						}
						adres.setGbaGemeente(gemeente);
						adres.setPostcodeCoordinaten(coordinatenService.getCoordinaten(adres));
						persoon.setGbaAdres(adres);

						if (StringUtils.isNotBlank(columns[headersIndex.get("DatumVertrekUitNederland")]))
						{
							persoon.setDatumVertrokkenUitNederland(new SimpleDateFormat("yyyyMMdd").parse(columns[headersIndex.get("DatumVertrekUitNederland")]));
						}

						persoon.setTelefoonnummer1(columns[headersIndex.get("TelNrPrive")]);
						persoon.setTelefoonnummer2(columns[headersIndex.get("TelNrWerk")]);
						persoon.setEmailadres(columns[headersIndex.get("EmailAdres")]);

						clientService.saveOrUpdateClient(client);

						importBvoViaCsv.importBvoViaCsv(headersIndex, columns, client);

						clientService.saveOrUpdateClient(client);
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
			}
		}
	}

	@Override
	@Transactional
	public boolean verwijderClientContacten(Client client, Bevolkingsonderzoek... onderzoeken)
	{
		return verwijderClientContacten(client, Arrays.asList(onderzoeken));
	}

	@Override
	@Transactional
	public boolean verwijderClientContacten(Client client, boolean isDossierVerwijderdDK, boolean isDossierVerwijderdBMHK, boolean isDossierVerwijderdBK)
	{
		var onderzoeken = new ArrayList<Bevolkingsonderzoek>();
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
	@Transactional
	public boolean verwijderClientContacten(Client client, List<Bevolkingsonderzoek> onderzoeken)
	{

		if (CollectionUtils.isNotEmpty(client.getContacten()))
		{
			var verwijderdeContactenLijst = new ArrayList<ClientContact>();
			for (var contact : client.getContacten())
			{
				var clientContactMagWeg = true;
				var verwijderenContactActieLijst = new ArrayList<ClientContactActie>();
				for (var actie : contact.getActies())
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
	@Transactional
	public void projectenVerwijderen(Client client)
	{
		for (var pclient : client.getProjecten())
		{
			var projectBrieven = pclient.getBrieven();
			projectBrieven.forEach(projectBrief ->
			{
				var clientBrief = projectBrief.getBrief();
				if (clientBrief != null)
				{
					clientBrief.setProjectBrief(null);
				}
				var mergedBrieven = projectBrief.getMergedBrieven();
				if (mergedBrieven != null)
				{
					mergedBrieven.getBrieven().remove(projectBrief);
					hibernateService.saveOrUpdate(mergedBrieven);
				}
			});

			var projectInactiveerDocument = pclient.getProjectInactiveerDocument();
			if (projectInactiveerDocument != null)
			{
				var projectClienten = projectInactiveerDocument.getProjectClienten();
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
			var groep = pclient.getGroep();
			groep.setPopulatie(groep.getPopulatie() - 1);
			groep.getClienten().remove(pclient);
			hibernateService.saveOrUpdateAll(groep);
			hibernateService.delete(pclient);
		}
		hibernateService.saveOrUpdate(client);
	}

	@Override
	public BMHKLaboratorium getEersteBMHKLaboratorium()
	{
		return bmhkLaboratoriumRepository.findFirst(null, Sort.by(SingleTableHibernateObject_.ID)).orElse(null);
	}

	@Override
	public Gemeente getGemeenteByCode(String code)
	{
		return gemeenteRepository.findOneByCode(code).orElse(null);
	}

	@Override
	public Gemeente getEersteGemeenteMetScreeningOrganisatie()
	{
		return gemeenteRepository.findFirst(heeftScreeningOrganisatie(), Sort.by(Sort.Order.asc(Gemeente_.NAAM))).orElse(null);
	}

	@Override
	public List<Gemeente> getGemeentesMetScreeningOrganisatie()
	{
		return gemeenteRepository.findAll(heeftScreeningOrganisatie(), Sort.by(Sort.Order.asc(Gemeente_.NAAM)));
	}

	@Override
	public Gemeente getEersteGemeenteMetNaam(String naam)
	{
		return gemeenteRepository.findFirst(GemeenteSpecification.heeftNaam(naam), Sort.by(AbstractHibernateObject_.ID)).orElse(null);
	}

	@Override
	public Instelling getOrganisatieByNaam(String naam)
	{
		return organisatieRepository.findOne(OrganisatieSpecification.heeftNaam(naam).and(OrganisatieSpecification.isActief(true))).orElse(null);
	}
}
