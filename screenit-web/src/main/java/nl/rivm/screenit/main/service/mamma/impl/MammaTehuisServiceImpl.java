package nl.rivm.screenit.main.service.mamma.impl;

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

import java.text.Normalizer;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.dao.mamma.MammaBaseTehuisClientenDao;
import nl.rivm.screenit.main.service.mamma.MammaTehuisService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.IDocument;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.mamma.MammaBrief;
import nl.rivm.screenit.model.mamma.MammaMergedBrieven;
import nl.rivm.screenit.model.mamma.MammaTehuis;
import nl.rivm.screenit.model.mamma.MammaTehuisOpmerking;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.model.mamma.enums.MammaUitstelGeannuleerdReden;
import nl.rivm.screenit.model.mamma.enums.MammaUitstelReden;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.impl.IBrievenGeneratorHelper;
import nl.rivm.screenit.service.mamma.MammaBaseFactory;
import nl.rivm.screenit.service.mamma.MammaBaseKansberekeningService;
import nl.rivm.screenit.service.mamma.MammaBaseScreeningrondeService;
import nl.rivm.screenit.service.mamma.MammaBaseTehuisService;
import nl.rivm.screenit.service.mamma.MammaBaseUitstelService;
import nl.rivm.screenit.service.mamma.MammaVolgendeUitnodigingService;
import nl.rivm.screenit.service.mamma.enums.MammaTehuisSelectie;
import nl.rivm.screenit.util.EntityAuditUtil;
import nl.rivm.screenit.util.mamma.MammaScreeningRondeUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Slf4j
public class MammaTehuisServiceImpl implements MammaTehuisService
{
	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private LogService logService;

	@Autowired
	private MammaBaseTehuisClientenDao baseTehuisClientenDao;

	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Autowired
	private MammaBaseFactory baseFactory;

	@Autowired
	private BaseBriefService briefService;

	@Autowired
	private MammaBaseTehuisService baseTehuisService;

	@Autowired
	private MammaBaseScreeningrondeService baseScreeningrondeService;

	@Autowired
	private MammaBaseUitstelService baseUitstelService;

	@Autowired
	private ClientService clientService;

	@Autowired
	private MammaBaseKansberekeningService baseKansberekeningService;

	@Autowired
	private MammaVolgendeUitnodigingService volgendeUitnodigingService;

	private static final BriefType BRIEF_TYPE_TEHUIS_UITNODIGING = BriefType.MAMMA_UITNODIGING_TEHUIS_ZONDER_DATUM;

	private static final BriefType BRIEF_TYPE_SUSPECT_UITNODIGING = BriefType.MAMMA_UITNODIGING_SUSPECT;

	@Transactional
	@Override
	public void deactiveerTehuis(MammaTehuis tehuis, InstellingGebruiker instellingGebruiker)
	{
		for (var dossier : tehuis.getDossiers())
		{
			dossier.setTehuis(null);
			hibernateService.saveOrUpdate(dossier);

			baseKansberekeningService.dossierEventHerzien(dossier);
		}
		baseTehuisService.saveOrUpdateTehuis(tehuis, instellingGebruiker);
	}

	@Transactional
	@Override
	public boolean saveOrUpdateTehuisOpmerking(MammaTehuisOpmerking opmerking, MammaTehuis tehuis, InstellingGebruiker loggedInInstellingGebruiker)
	{
		opmerking.setCreatieDatum(dateSupplier.getDate());
		if (opmerking.getId() == null && tehuis != null)
		{
			opmerking.setTehuis(tehuis);
			tehuis.getOpmerkingen().add(opmerking);
		}
		if (tehuis == null)
		{
			tehuis = opmerking.getTehuis();
		}

		var melding = "";
		var diffToLatestVersion = EntityAuditUtil.getDiffToLatestVersion(opmerking, hibernateService.getHibernateSession());

		if (opmerking.getId() == null)
		{
			melding += "Opmerking voor tehuis '" + tehuis.getNaam() + "' aangemaakt.";
		}
		else if (diffToLatestVersion.length() > 0)
		{
			melding += "Opmerking voor tehuis '" + tehuis.getNaam() + "' gewijzigd.";
		}
		if (StringUtils.isNotBlank(melding))
		{
			logService.logGebeurtenis(LogGebeurtenis.MAMMA_TEHUIS_BEHEER, loggedInInstellingGebruiker, melding, Bevolkingsonderzoek.MAMMA);
			hibernateService.saveOrUpdateAll(opmerking, tehuis);
			return true;
		}
		return false;
	}

	@Transactional
	@Override
	public void uitnodigen(MammaTehuis tehuis, AtomicInteger aantalClientenMetProjectBrief, AtomicInteger aantalClientenMetBrieven,
		AtomicInteger aantalClientenMetSuspectBrieven, InstellingGebruiker ingelogdeInstellingGebruiker)
	{
		var standplaatsRonde = baseTehuisService.getHuidigeStandplaatsRondeVoorStandplaats(tehuis.getStandplaats());
		List<MammaBrief> uitnodigingen = new ArrayList<>();
		List<MammaBrief> suspectUitnodigingen = new ArrayList<>();

		var clienten = baseTehuisClientenDao.getClienten(tehuis, MammaTehuisSelectie.UIT_TE_NODIGEN, null);
		for (var client : clienten)
		{
			var dossier = client.getMammaDossier();
			var krijgtSuspectBrief = volgendeUitnodigingService.isSuspect(dossier);

			var screeningRonde = baseFactory.maakRonde(dossier, standplaatsRonde, false);

			var briefType = krijgtSuspectBrief ? BRIEF_TYPE_SUSPECT_UITNODIGING : BRIEF_TYPE_TEHUIS_UITNODIGING;
			var uitnodiging = baseFactory.maakUitnodiging(screeningRonde, standplaatsRonde, briefType);

			var brief = uitnodiging.getBrief();
			if (!brief.isVervangendeProjectBrief())
			{
				if (krijgtSuspectBrief)
				{
					suspectUitnodigingen.add(brief);
				}
				else
				{
					uitnodigingen.add(brief);
				}
			}
			else
			{
				aantalClientenMetProjectBrief.incrementAndGet();
			}
		}
		var nu = dateSupplier.getDate();
		if (!uitnodigingen.isEmpty() || aantalClientenMetProjectBrief.get() > 0)
		{
			var melding = uitnodigingen.size() + suspectUitnodigingen.size() + aantalClientenMetProjectBrief.get() + " cliënten uitgenodigd";
			if (suspectUitnodigingen.size() + aantalClientenMetProjectBrief.get() > 0)
			{
				melding += " waarvan ";
				if (!suspectUitnodigingen.isEmpty())
				{
					melding += suspectUitnodigingen.size() + " suspect cliënten ";
				}
				if (aantalClientenMetProjectBrief.get() > 0)
				{
					melding += aantalClientenMetProjectBrief.get() + " project cliënten";
				}
			}

			var opmerking = new MammaTehuisOpmerking();
			opmerking.setCreatieDatum(nu);
			opmerking.setActief(true);
			opmerking.setOpmerking(melding);
			opmerking.setTehuis(tehuis);
			tehuis.setUitgenodigd(nu);
			tehuis.getOpmerkingen().add(opmerking);
			hibernateService.saveOrUpdateAll(opmerking, tehuis);
			logService.logGebeurtenis(LogGebeurtenis.MAMMA_TEHUIS_UITGENODIGD, ingelogdeInstellingGebruiker,
				"'" + tehuis.getNaam() + "' met " + tehuis.getDossiers().size() + " clienten. " + melding, Bevolkingsonderzoek.MAMMA);
		}
		if (!uitnodigingen.isEmpty())
		{
			genereerBrieven(tehuis, uitnodigingen, aantalClientenMetBrieven, BRIEF_TYPE_TEHUIS_UITNODIGING);
		}

		if (!suspectUitnodigingen.isEmpty())
		{
			genereerBrieven(tehuis, suspectUitnodigingen, aantalClientenMetSuspectBrieven, BRIEF_TYPE_SUSPECT_UITNODIGING);
		}
	}

	@Override
	public List<String> koppelClient(MammaTehuis tehuis, Client client)
	{
		List<String> meldingen = new ArrayList<>();
		var dossier = client.getMammaDossier();

		var screeningRonde = dossier.getLaatsteScreeningRonde();

		var uitstel = screeningRonde != null ? screeningRonde.getLaatsteUitstel() : null;

		if (uitstel != null && uitstel.getGeannuleerdOp() == null && uitstel.getUitnodiging() == null)
		{
			if (uitstel.getUitstelReden() == MammaUitstelReden.ACHTERVANG_UITSTEL || uitstel.getUitstelReden() == MammaUitstelReden.MINDER_VALIDE_UITWIJK_UITSTEL)
			{
				baseScreeningrondeService.verwijderScreeningRonde(screeningRonde, false);
			}
			else
			{
				baseUitstelService.uitstelAfzeggen(uitstel, MammaUitstelGeannuleerdReden.TEHUIS_KOPPELING, dateSupplier.getDate());
				logService.logGebeurtenis(LogGebeurtenis.MAMMA_UITSTEL_GEANNULEERD_TEHUIS, ScreenitSession.get().getLoggedInAccount(), client);
				meldingen.add("client.gekoppeld.uitstel.geannuleerd");
			}
		}

		var laatsteAfspraak = MammaScreeningRondeUtil.getLaatsteAfspraak(screeningRonde);
		if (laatsteAfspraak != null && laatsteAfspraak.getStatus().equals(MammaAfspraakStatus.GEPLAND) && laatsteAfspraak.getVanaf().compareTo(dateSupplier.getDate()) >= 0)
		{
			meldingen.add("client.gekoppeld.heeft.afspraak");
		}

		dossier.setTehuis(tehuis);
		tehuis.getDossiers().add(dossier);
		hibernateService.saveOrUpdateAll(dossier, tehuis);

		baseKansberekeningService.dossierEventHerzien(dossier);
		return meldingen;
	}

	@Override
	public List<String> ontkoppelClient(MammaTehuis tehuis, Client client)
	{
		List<String> meldingen = new ArrayList<>();
		var dossier = client.getMammaDossier();
		var screeningRonde = dossier.getLaatsteScreeningRonde();

		var laatsteAfspraak = MammaScreeningRondeUtil.getLaatsteAfspraak(screeningRonde);
		if (laatsteAfspraak != null && laatsteAfspraak.getStatus().equals(MammaAfspraakStatus.GEPLAND) && laatsteAfspraak.getVanaf().compareTo(dateSupplier.getDate()) >= 0)
		{
			meldingen.add("client.ontkoppeld.heeft.afspraak");
		}

		dossier.setTehuis(null);
		tehuis.getDossiers().remove(dossier);
		hibernateService.saveOrUpdateAll(dossier, tehuis);

		baseKansberekeningService.dossierEventHerzien(dossier);
		return meldingen;
	}

	private void genereerBrieven(MammaTehuis tehuis, List<MammaBrief> uitnodigingen, AtomicInteger aantalClienten, BriefType briefType)
	{
		var nu = dateSupplier.getDate();

		var mergedBrieven = new MammaMergedBrieven();
		mergedBrieven.setScreeningOrganisatie(tehuis.getStandplaats().getRegio());
		mergedBrieven.setCreatieDatum(nu);
		mergedBrieven.setBriefType(briefType);
		mergedBrieven.setActief(false);
		mergedBrieven.setAantalBrieven(0);
		mergedBrieven.setVrijgegeven(true);
		hibernateService.saveOrUpdate(mergedBrieven);

		try
		{
			briefService.createOrAddMergedBrieven(uitnodigingen, new TehuisBrievenGeneratorHelper(tehuis, mergedBrieven));
			briefService.completePdf(mergedBrieven);
			aantalClienten.set(uitnodigingen.size());
		}
		catch (Exception e)
		{
			LOG.error("Onbekende fout bij mergen van tehuis brieven", e);
			throw new IllegalStateException(e);
		}
	}

	private class TehuisBrievenGeneratorHelper implements IBrievenGeneratorHelper<MammaBrief, MammaMergedBrieven>
	{
		private final MammaTehuis tehuis;

		private final MammaMergedBrieven mergedBrieven;

		TehuisBrievenGeneratorHelper(MammaTehuis tehuis, MammaMergedBrieven mergedBrieven)
		{
			this.tehuis = tehuis;
			this.mergedBrieven = mergedBrieven;

		}

		@Override
		public String getMergedBrievenNaam(MammaMergedBrieven brieven)
		{
			var naam = "";
			var sdf = new SimpleDateFormat("yyyy-MM-dd_HH.mm");
			if (brieven.getCreatieDatum() != null)
			{
				naam += sdf.format(brieven.getCreatieDatum()) + "-";
			}
			if (brieven.getScreeningOrganisatie() != null)
			{
				var soNaam = brieven.getScreeningOrganisatie().getNaam();
				soNaam = soNaam.replace(" ", "_");
				naam += soNaam + "-";
			}
			naam += Normalizer
				.normalize(tehuis.getNaam(), Normalizer.Form.NFD)
				.replaceAll("[^\\p{ASCII}]", "_").replaceAll("[^a-zA-Z0-9 ]+", "_") + "-";
			if (brieven.getBriefType() != null)
			{
				naam += brieven.getBriefType().name().toLowerCase();
			}
			return naam + ".pdf";
		}

		@Override
		public void verhoogAantalBrievenVanScreeningOrganisatie(MammaMergedBrieven mergedBrieven)
		{
		}

		@Override
		public void additionalMergedContext(MailMergeContext context)
		{
			var ce = clientService.bepaalCe(context.getClient());
			context.putValue(MailMergeContext.CONTEXT_MAMMA_CE, ce);
		}

		@Override
		public Bevolkingsonderzoek[] getBevolkingsonderzoeken()
		{
			return new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA };
		}

		@Override
		public LogGebeurtenis getMergeProbleemLogGebeurtenis()
		{
			return LogGebeurtenis.MAMMA_BRIEF_MERGE_FOUT;
		}

		@Override
		public LogGebeurtenis getOnvolledigAdresLogGebeurtenis()
		{
			return LogGebeurtenis.MAMMA_ONVOLLEDIG_ADRES;
		}

		@Override
		public FileStoreLocation getFileStoreLocation()
		{
			return FileStoreLocation.MAMMA_MERGED_BRIEVEN;
		}

		@Override
		public IDocument getDocumentDefinitie()
		{
			return briefService.getNieuwsteBriefDefinitie(mergedBrieven.getBriefType());
		}

		@Override
		public MammaMergedBrieven getMergedBrieven()
		{
			return mergedBrieven;
		}

	}
}
