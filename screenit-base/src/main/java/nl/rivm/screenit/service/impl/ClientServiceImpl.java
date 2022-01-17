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

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.dao.ClientDao;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.BagAdres;
import nl.rivm.screenit.model.CentraleEenheid;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientBrief;
import nl.rivm.screenit.model.Dossier;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.RedenOpnieuwAanvragenClientgegevens;
import nl.rivm.screenit.model.TijdelijkAdres;
import nl.rivm.screenit.model.TijdelijkGbaAdres;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.cervix.enums.CervixMonsterType;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.GbaStatus;
import nl.rivm.screenit.model.enums.GbaVraagType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.gba.GbaVraag;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.rivm.screenit.model.project.ProjectInactiefReden;
import nl.rivm.screenit.repository.ClientRepository;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.FileService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.mamma.MammaBaseStandplaatsService;
import nl.rivm.screenit.util.AdresUtil;
import nl.rivm.screenit.util.ProjectUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.math.NumberUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Component
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class ClientServiceImpl implements ClientService
{
	private static final Logger LOG = LoggerFactory.getLogger(ClientServiceImpl.class);

	@Autowired
	private ClientRepository clientRepository;

	@Autowired
	private ClientDao clientDao;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private FileService fileService;

	@Autowired
	private LogService logService;

	@Autowired(required = false)
	private MammaBaseStandplaatsService baseStandplaatsService;

	@Override
	public Client getClientByBsn(String bsn)
	{
		return clientRepository.findOne(ClientRepository.bsnEquals(bsn)).orElse(null);
	}

	@Override
	public Client getClientZonderBezwaar(String bsn)
	{
		return clientRepository.findOne(ClientRepository.bsnEquals(bsn).and(ClientRepository.gbaStatusNotEquals(GbaStatus.BEZWAAR))).orElse(null);
	}

	@Override
	public boolean heeftDossierMetRondeOfAfmelding(Client client)
	{
		return clientDao.heeftDossierMetRondeOfAfmelding(client);
	}

	@Override
	public Client setTelefoonnummer(Client client)
	{
		return client;
	}

	@Override
	public Client getClientByBsnFromNg01Bericht(String bsn, String anummer)
	{
		return clientDao.getClientByBsnFromNg01Bericht(bsn, anummer);
	}

	@Override
	public Client getLaatstAfgevoerdeClient(String bsn)
	{
		return clientDao.getLaatstAfgevoerdeClient(bsn);
	}

	@Override
	public String getVoorNg01EenNieuweBsn(String bsn)
	{
		if (bsn == null) 
		{
			return null;
		}

		Client client = getClientByBsnFromNg01Bericht(bsn, null);
		if (client != null && client.getPersoon().getBsn().length() == 12)
		{
			bsn = client.getPersoon().getBsn();
		}

		int ng01Value = 0;
		if (bsn != null && bsn.length() == 12)
		{
			ng01Value = Integer.parseInt(bsn.substring(0, 3));
			bsn = bsn.substring(3, bsn.length());
		}
		ng01Value++;

		return StringUtils.leftPad(ng01Value + "", 3, '0') + bsn;
	}

	@Override
	public List<Client> zoekClienten(Client zoekObject)
	{
		return clientDao.zoekClienten(zoekObject);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void saveOrUpdateClient(Client client)
	{
		TijdelijkAdres tijdelijkAdres = client.getPersoon().getTijdelijkAdres();
		if (tijdelijkAdres != null)
		{
			if (tijdelijkAdres.getStartDatum() == null && tijdelijkAdres.getEindDatum() == null || StringUtils.isBlank(tijdelijkAdres.getStraat()))
			{
				if (tijdelijkAdres.getId() != null)
				{
					hibernateService.delete(tijdelijkAdres);
				}
				client.getPersoon().setTijdelijkAdres(null);
			}
		}
		clientDao.saveOrUpdateClient(client);
	}

	@Override
	public List<Client> getClientenMetTitel(String titelCode)
	{
		return clientDao.getClientenMetTitel(titelCode);
	}

	@Override
	public Dossier getDossier(Client client, Bevolkingsonderzoek bevolkingsonderzoek)
	{
		switch (bevolkingsonderzoek)
		{
		case COLON:
			return client.getColonDossier();
		case CERVIX:
			return client.getCervixDossier();
		case MAMMA:
			return client.getMammaDossier();
		}
		return null;
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public CervixUitnodiging getLaatstVerstuurdeUitnodiging(CervixScreeningRonde ronde, boolean inclusiefZas)
	{
		CervixUitnodiging laatsteUitnodiging = null;
		for (CervixUitnodiging uitnodiging : ronde.getUitnodigingen())
		{
			if (laatsteUitnodiging == null || laatsteUitnodiging.getId() < uitnodiging.getId())
			{
				if (uitnodiging.getMonsterType() == CervixMonsterType.UITSTRIJKJE && uitnodiging.getBrief().isGegenereerd()
					|| inclusiefZas && uitnodiging.getMonsterType() == CervixMonsterType.ZAS && uitnodiging.getMonster() != null)
				{
					laatsteUitnodiging = uitnodiging;
				}
			}
		}
		return laatsteUitnodiging;
	}

	@Override
	public Client getClientByAnummer(String anummer)
	{
		return clientDao.getClientByANummer(anummer);
	}

	@Override
	public boolean heeftClientIntakeConclusieMetBezwaar(String bsn)
	{
		return clientDao.heeftClientIntakeConclusieMetBezwaar(bsn);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void saveDocumentForClient(UploadDocument uploadDocument, Client client)
	{
		List<UploadDocument> documents = client.getDocuments();
		try
		{
			fileService.saveOrUpdateUploadDocument(uploadDocument, FileStoreLocation.CLIENT_DOCUMENTEN, client.getId());
			documents.add(uploadDocument);
			client.setDocuments(documents);
			hibernateService.saveOrUpdate(client);
		}
		catch (IOException e)
		{
			LOG.error("Er is een fout opgetreden! " + e.getMessage(), e);
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void deleteDocumentForClient(UploadDocument document, Client client)
	{
		fileService.deleteDocumentFromList(document, client.getDocuments());
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void actiesNaUpdateWithGba(Client client)
	{
		if (client.getPersoon() != null)
		{
			GbaPersoon persoon = client.getPersoon();
			if (persoon.getOverlijdensdatum() != null)
			{
				alleProjectClientenInactiveren(client, ProjectInactiefReden.OVERLEDEN, null);
			}

			if (persoon.getDatumVertrokkenUitNederland() != null)
			{
				alleProjectClientenInactiveren(client, ProjectInactiefReden.VERTROKKEN_UIT_NEDERLAND, null);
			}
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void projectClientInactiveren(ProjectClient pClient, ProjectInactiefReden reden, Bevolkingsonderzoek bevolkingsonderzoek)
	{
		if (pClient != null)
		{
			if (!reden.equals(ProjectInactiefReden.AFMELDING)
				|| reden.equals(ProjectInactiefReden.AFMELDING) && pClient.getProject().getExcludeerAfmelding().contains(bevolkingsonderzoek))
			{
				pClient.setActief(false);
				pClient.setProjectInactiefReden(reden);
				pClient.setProjectInactiefDatum(currentDateSupplier.getDate());
				hibernateService.saveOrUpdate(pClient);

				String melding = String.format("Project: %s, groep: %s, reden: %s",
					pClient.getProject().getNaam(),
					pClient.getGroep().getNaam(),
					reden.naam);

				logService.logGebeurtenis(LogGebeurtenis.PROJECTCLIENT_GEINACTIVEERD, pClient.getClient(), melding);
			}
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void alleProjectClientenInactiveren(Client client, ProjectInactiefReden projectInactiefReden, Bevolkingsonderzoek bvo)
	{
		List<ProjectClient> projectClienten = ProjectUtil.getHuidigeProjectClienten(client, currentDateSupplier.getDate(), false);
		if (projectClienten != null && projectClienten.size() > 0)
		{
			for (ProjectClient projectClient : projectClienten)
			{
				projectClientInactiveren(projectClient, projectInactiefReden, bvo);
			}
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void projectClientInactiveren(Client client, ProjectInactiefReden reden, Bevolkingsonderzoek bevolkingsonderzoek)
	{
		ProjectClient pClient = ProjectUtil.getHuidigeProjectClient(client, currentDateSupplier.getDate());
		if (pClient != null)
		{
			projectClientInactiveren(pClient, reden, bevolkingsonderzoek);
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public String projectClientActiveren(ProjectClient projectClient)
	{

		if (ProjectUtil.getHuidigeProjectClient(projectClient.getClient(), currentDateSupplier.getDate()) != null)
		{
			return "ander.project.opgenomen";
		}
		projectClient.setActief(Boolean.TRUE);
		if (!isErEenUitnodigingAangemaaktInProjectPeriode(projectClient))
		{
			projectClient.setIsUitgenodigdInProjectPeriode(Boolean.TRUE);
		}
		hibernateService.saveOrUpdate(projectClient);
		return null;
	}

	private boolean isErEenUitnodigingAangemaaktInProjectPeriode(ProjectClient projectClient)
	{
		Date beginDatum = projectClient.getToegevoegd();
		List<ColonUitnodiging> uitnodigingen = clientDao.getAllColonUitnodigingenVanClientInPeriode(projectClient.getClient(), beginDatum,
			currentDateSupplier.getDate());
		return CollectionUtils.isNotEmpty(uitnodigingen);
	}

	@Override
	public boolean isClientOverleden(Client client)
	{
		return client != null && client.getPersoon() != null && client.getPersoon().getOverlijdensdatum() != null;
	}

	@Override
	public boolean clientInBuitenland(Client client)
	{
		return client != null && client.getPersoon() != null && client.getPersoon().getDatumVertrokkenUitNederland() != null;
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public GbaVraag vraagGbaGegevensOpnieuwAan(Client client, Account account, RedenOpnieuwAanvragenClientgegevens reden)
	{
		GbaVraag gbaVraag = new GbaVraag();
		gbaVraag.setClient(client);
		gbaVraag.setDatum(currentDateSupplier.getDate());
		gbaVraag.setVraagType(GbaVraagType.VERWIJDER_INDICATIE);
		gbaVraag.setReden(reden);
		gbaVraag.setReactieOntvangen(false);

		hibernateService.saveOrUpdate(gbaVraag);

		client.setGbaStatus(GbaStatus.INDICATIE_VERWIJDERD);
		hibernateService.saveOrUpdate(client);
		String melding = "Reden: ";
		if (account != null)
		{
			melding += reden.getNaam();
		}
		else
		{
			melding += "Retourzending";
		}
		logService.logGebeurtenis(LogGebeurtenis.GBA_GEGEVENS_OPNIEUW_AANGEVRAAGD, getScreeningOrganisatieVan(client), account, client, melding);
		return gbaVraag;
	}

	@Override
	public List<Instelling> getScreeningOrganisatieVan(Client client)
	{
		if (client != null && client.getPersoon().getGbaAdres() != null && client.getPersoon().getGbaAdres().getGbaGemeente() != null
			&& client.getPersoon().getGbaAdres().getGbaGemeente().getScreeningOrganisatie() != null)
		{
			return Arrays.asList(client.getPersoon().getGbaAdres().getGbaGemeente().getScreeningOrganisatie());
		}
		return new ArrayList<>();
	}

	@Override
	public boolean isTijdelijkeAdresNuActueel(GbaPersoon persoon)
	{
		return AdresUtil.isTijdelijkAdres(persoon, currentDateSupplier.getDateTime());
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void saveOrUpdateTijdelijkGbaAdres(Client client, InstellingGebruiker ingelogdeGebruiker)
	{
		String melding = "Gewijzigd.";
		GbaPersoon persoon = client.getPersoon();
		TijdelijkGbaAdres tijdelijkGbaAdres = persoon.getTijdelijkGbaAdres();
		if (tijdelijkGbaAdres != null && tijdelijkGbaAdres.getId() == null)
		{
			melding = "Aangemaakt.";
		}
		logService.logGebeurtenis(LogGebeurtenis.GBA_TIJDELIJK_ADRES, ingelogdeGebruiker, client, melding);
		hibernateService.saveOrUpdate(persoon);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void verwijderTijdelijkGbaAdres(Client client, InstellingGebruiker ingelogdeGebruiker)
	{
		String melding = "Handmatig verwijderd.";
		GbaPersoon persoon = client.getPersoon();
		TijdelijkGbaAdres tijdelijkGbaAdres = persoon.getTijdelijkGbaAdres();
		persoon.setTijdelijkGbaAdres(null);
		hibernateService.delete(tijdelijkGbaAdres);
		logService.logGebeurtenis(LogGebeurtenis.GBA_TIJDELIJK_ADRES, ingelogdeGebruiker, client, melding);
		hibernateService.saveOrUpdate(persoon);
	}

	@Override
	public String valideerBriefkenmerk(String briefkenmerk, Client zoekClient)
	{
		String errorString = null;
		try
		{

			briefkenmerk = briefkenmerk.replaceAll(" ", "").toUpperCase();
			if (briefkenmerk.startsWith("K"))
			{
				Client client = getClientMetBriefkenmerk(briefkenmerk);

				boolean heeftBriefPersoon = client != null && client.getPersoon() != null;
				boolean heeftZoekClientPersoon = zoekClient != null && zoekClient.getPersoon() != null;
				boolean kloptIngevoerdeGeboortedatum = heeftBriefPersoon && heeftZoekClientPersoon
					&& zoekClient.getPersoon().getGeboortedatum().equals(client.getPersoon().getGeboortedatum());
				if (heeftBriefPersoon && heeftZoekClientPersoon)
				{
					GbaPersoon zoekPersoon = zoekClient.getPersoon();
					GbaPersoon briefPersoon = client.getPersoon();

					boolean kloptIngevoerdeBsn = zoekPersoon.getBsn() == null
						|| zoekPersoon.getBsn() != null && zoekPersoon.getBsn().equals(briefPersoon.getBsn());

					BagAdres zoekAdres = zoekPersoon.getGbaAdres();
					boolean kloptIngevoerdePostcodeEnHuisNr = zoekAdres == null
						|| zoekAdres.getPostcode() == null && zoekAdres.getHuisnummer() == null
						|| zoekAdres.getPostcode() != null && zoekAdres.getHuisnummer() != null
							&& zoekAdres.getPostcode().equals(briefPersoon.getGbaAdres().getPostcode())
							&& zoekAdres.getHuisnummer().equals(briefPersoon.getGbaAdres().getHuisnummer());

					if (kloptIngevoerdeGeboortedatum && kloptIngevoerdeBsn && kloptIngevoerdePostcodeEnHuisNr)
					{
						return null;
					}
				}
				errorString = "error.briefkenmerk.combi.niet.gevonden";
			}
			else
			{
				errorString = "error.onjuiste.briefkenmerkformat";
			}
		}
		catch (NumberFormatException e)
		{
			errorString = "error.onjuiste.briefkenmerk";
		}
		return errorString;
	}

	@Override
	public Client getClientMetBriefkenmerk(String briefkenmerk)
	{
		Client client = null;

		briefkenmerk = briefkenmerk.replaceAll(" ", "").toUpperCase();
		if (briefkenmerk.startsWith("KU"))
		{ 
			briefkenmerk = briefkenmerk.substring(2);
			if (NumberUtils.isCreatable("0x" + briefkenmerk))
			{
				Map<String, Object> parameters = new HashMap<>();
				parameters.put("uitnodigingsId", Long.parseLong(briefkenmerk, 16));
				ColonUitnodiging uitnodiging = hibernateService.getUniqueByParameters(ColonUitnodiging.class, parameters);
				if (uitnodiging != null)
				{
					client = uitnodiging.getScreeningRonde().getDossier().getClient();
				}
			}
		}
		else
		{ 
			briefkenmerk = briefkenmerk.substring(1);
			if (NumberUtils.isCreatable("0x" + briefkenmerk))
			{
				ClientBrief brief = hibernateService.get(ClientBrief.class, Long.parseLong(briefkenmerk, 16));
				if (brief != null)
				{
					client = brief.getClient();
				}
			}
		}
		if (client != null && !client.getGbaStatus().equals(GbaStatus.BEZWAAR) && !client.getGbaStatus().equals(GbaStatus.AFGEVOERD))
		{
			return client;
		}
		return null;
	}

	@Override
	public CentraleEenheid bepaalCe(Client client)
	{
		MammaStandplaatsPeriode periode = baseStandplaatsService
			.getEerstvolgendeStandplaatsPeriode(baseStandplaatsService.getStandplaatsMetPostcode(client));
		if (periode == null && client.getMammaDossier().getLaatsteScreeningRonde() != null)
		{
			MammaScreeningRonde screeningRonde = client.getMammaDossier().getLaatsteScreeningRonde();
			if (screeningRonde.getLaatsteUitnodiging() != null && screeningRonde.getLaatsteUitnodiging().getLaatsteAfspraak() != null)
			{
				periode = screeningRonde.getLaatsteUitnodiging().getLaatsteAfspraak().getStandplaatsPeriode();
			}
		}

		if (periode != null)
		{
			return (CentraleEenheid) HibernateHelper.deproxy(periode.getScreeningsEenheid().getBeoordelingsEenheid().getParent());
		}
		return null;
	}

	@Override
	public String getGbaPostcode(Client client)
	{
		return client.getPersoon().getTijdelijkGbaAdres() != null ? client.getPersoon().getTijdelijkGbaAdres().getPostcode()
			: client.getPersoon().getGbaAdres().getPostcode();
	}

	@Override
	public boolean isHandtekeningBriefGebruiktBijMeedereColonAfmeldingen(UploadDocument handtekeningBrief, String handtekeningProperty)
	{
		return clientDao.countUsedColonHandtekeningBrief(handtekeningBrief, handtekeningProperty) > 1;
	}
}
