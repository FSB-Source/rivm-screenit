package nl.rivm.screenit.service.cervix.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.text.SimpleDateFormat;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ScreeningRonde_;
import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.cervix.CervixUitstel;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.CervixZas;
import nl.rivm.screenit.model.cervix.enums.CervixLabformulierStatus;
import nl.rivm.screenit.model.cervix.enums.CervixMonsterType;
import nl.rivm.screenit.model.cervix.enums.CervixUitstrijkjeStatus;
import nl.rivm.screenit.model.cervix.enums.CervixZasStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.repository.algemeen.ClientRepository;
import nl.rivm.screenit.repository.cervix.CervixBetaalopdrachtRegelSpecificatieRepository;
import nl.rivm.screenit.repository.cervix.CervixBriefRepository;
import nl.rivm.screenit.repository.cervix.CervixScreeningRondeRepository;
import nl.rivm.screenit.repository.cervix.CervixUitnodigingRepository;
import nl.rivm.screenit.repository.cervix.CervixUitstelRepository;
import nl.rivm.screenit.service.BaseDossierService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.service.cervix.Cervix2023StartBepalingService;
import nl.rivm.screenit.service.cervix.CervixBaseScreeningrondeService;
import nl.rivm.screenit.service.cervix.CervixVervolgService;
import nl.rivm.screenit.service.cervix.enums.CervixVervolgTekst;
import nl.rivm.screenit.specification.cervix.CervixScreeningRondeSpecification;
import nl.rivm.screenit.specification.cervix.CervixUitnodigingSpecification;
import nl.rivm.screenit.util.BriefUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.cervix.CervixMonsterUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import static nl.rivm.screenit.specification.algemeen.ScreeningRondeSpecification.isAangemaaktVoor;
import static nl.rivm.screenit.specification.cervix.CervixScreeningRondeSpecification.heeftMonsterInDossier;
import static org.apache.commons.collections.CollectionUtils.isNotEmpty;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class CervixBaseScreeningrondeServiceImpl implements CervixBaseScreeningrondeService
{

	@Autowired
	private LogService logService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Autowired
	private CervixVervolgService vervolgService;

	@Autowired
	private BaseDossierService baseDossierService;

	@Autowired
	private UploadDocumentService uploadDocumentService;

	@Autowired
	private CervixScreeningRondeRepository screeningRondeRepository;

	@Autowired
	private CervixUitnodigingRepository uitnodigingRepository;

	@Autowired
	private CervixUitstelRepository uitstelRepository;

	@Autowired
	private ClientRepository clientRepository;

	@Autowired
	private CervixBriefRepository briefRepository;

	@Autowired
	private CervixBetaalopdrachtRegelSpecificatieRepository betaalopdrachtRegelSpecificatieRepository;

	@Autowired
	private Cervix2023StartBepalingService bmhk2023StartBepalingService;

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void annuleerNietVerstuurdeZAS(CervixScreeningRonde ronde)
	{
		for (var anderUitnodiging : ronde.getUitnodigingen())
		{
			if (anderUitnodiging.getMonsterType() == CervixMonsterType.ZAS)
			{
				if (anderUitnodiging.getVerstuurdDatum() == null)
				{
					if (anderUitnodiging.getGeannuleerdDatum() == null)
					{
						anderUitnodiging.setGeannuleerdDatum(DateUtil.toUtilDate(dateSupplier.getLocalDateTime().minusSeconds(1)));
						uitnodigingRepository.save(anderUitnodiging);
						break;
					}
				}
			}
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void uitstelAanvragen(Client client, CervixUitstel uitstel, Account account)
	{
		var wijziging = client.getCervixDossier().getLaatsteScreeningRonde().getUitstel() != null;

		uitstel.setGeannuleerdDatum(null);
		uitstel.setWijzigingsDatum(currentDateSupplier.getDate());

		var ronde = client.getCervixDossier().getLaatsteScreeningRonde();
		ronde.setUitstel(uitstel);
		uitstelRepository.save(uitstel);
		clientRepository.save(client);

		annuleerHerinnering(ronde);
		annuleerNietVerstuurdeZAS(ronde);

		var laatsteBrief = client.getCervixDossier().getLaatsteScreeningRonde().getLaatsteBrief();
		if (laatsteBrief != null && !BriefUtil.isGegenereerd(laatsteBrief))
		{
			briefRepository.save((CervixBrief) BriefUtil.setTegenhouden(laatsteBrief, true));
		}

		logService.logGebeurtenis(wijziging ? LogGebeurtenis.UITSTEL_GEWIJZIGD : LogGebeurtenis.UITSTEL_AANGEVRAAGD, account, client,
			maakCervixUitstelMelding(uitstel, wijziging), Bevolkingsonderzoek.CERVIX);

	}

	private String maakCervixUitstelMelding(CervixUitstel uitstel, boolean wijziging)
	{
		var sb = new StringBuilder();
		sb.append("Uitstel baarmoederhalskanker tot: ");
		sb.append(new SimpleDateFormat("dd-MM-yyyy").format(uitstel.getUitstellenTotDatum()));
		if (wijziging)
		{
			sb.append(" (wijziging)");
		}
		return sb.toString();
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void annuleerUitstel(CervixScreeningRonde ronde)
	{
		var uitstel = ronde.getUitstel();
		if (uitstel != null)
		{
			if (uitstel.getGeannuleerdDatum() == null)
			{
				uitstel.setGeannuleerdDatum(DateUtil.minusTijdseenheid(dateSupplier.getDate(), 50, ChronoUnit.MILLIS));
				uitstelRepository.save(uitstel);
			}
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void annuleerHerinnering(CervixScreeningRonde ronde)
	{
		var uitnodigingen = getTeHerinnerenUitnodigingen(ronde);
		for (var uitnodiging : uitnodigingen)
		{
			if (uitnodiging.getHerinnerenGeannuleerdDatum() == null)
			{
				uitnodiging.setHerinnerenGeannuleerdDatum(DateUtil.minusTijdseenheid(dateSupplier.getDate(), 50, ChronoUnit.MILLIS));
			}
		}
	}

	private List<CervixUitnodiging> getTeHerinnerenUitnodigingen(CervixScreeningRonde ronde)
	{
		return uitnodigingRepository.findAll(CervixUitnodigingSpecification
			.heeftScreeningRonde(ronde)
			.and(CervixUitnodigingSpecification.heeftHerinneren(true))
			.and(CervixUitnodigingSpecification.heeftGeenGeanulleerdeHerinneringDatum()));
	}

	@Override
	public boolean heeftUitslagOfHeeftGehad(CervixUitnodiging cervixUitnodiging)
	{
		var zas = CervixMonsterUtil.getZAS(cervixUitnodiging.getMonster());
		return zas != null && zas.getZasStatus() != CervixZasStatus.VERSTUURD
			|| cervixUitnodiging.getScreeningRonde().getDossier().getLaatsteScreeningRonde().getMonsterHpvUitslag() != null;
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void verwijderScreeningRondes(CervixDossier dossier)
	{
		var rondes = dossier.getScreeningRondes();
		dossier.setLaatsteScreeningRonde(null);
		dossier.setScreeningRondes(new ArrayList<>());
		if (isNotEmpty(rondes))
		{
			for (var ronde : rondes)
			{
				verwijderScreeningRonde(ronde);
			}
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void verwijderScreeningRonde(CervixScreeningRonde ronde)
	{
		baseDossierService.verwijderAlleAfmeldingenUitRonde(ronde);
		var uitnodigingen = ronde.getUitnodigingen();
		for (var uitnodiging : uitnodigingen)
		{
			if (uitnodiging.getMonster() != null)
			{
				var verwijderdBrief = uitnodiging.getMonster().getVerwijderdBrief();
				if (verwijderdBrief != null)
				{
					uploadDocumentService.delete(verwijderdBrief);
				}
				var monster = uitnodiging.getMonster();
				for (var verrichting : monster.getVerrichtingen())
				{
					for (var boekRegel : verrichting.getBoekRegels())
					{
						boekRegel.setVerrichting(null);
						hibernateService.delete(boekRegel);
						if (boekRegel.getSpecificatie() != null)
						{
							var specificatie = boekRegel.getSpecificatie();
							specificatie.getBoekRegels().remove(boekRegel);
							betaalopdrachtRegelSpecificatieRepository.save(specificatie);
						}
					}
					hibernateService.delete(verrichting);
				}
				for (var hpvBeoordeling : monster.getHpvBeoordelingen())
				{
					hibernateService.delete(hpvBeoordeling);
				}
				switch (uitnodiging.getMonsterType())
				{
				case UITSTRIJKJE:
					var uitstrijkje = (CervixUitstrijkje) HibernateHelper.deproxy(monster);
					if (uitstrijkje != null)
					{
						if (uitstrijkje.getLabformulier() != null)
						{
							hibernateService.delete(uitstrijkje.getLabformulier());
						}
						if (uitstrijkje.getCytologieVerslag() != null)
						{
							hibernateService.delete(uitstrijkje.getCytologieVerslag());
						}
						if (uitstrijkje.getCytologieOrder() != null)
						{
							hibernateService.delete(uitstrijkje.getCytologieOrder());
						}
						hibernateService.delete(uitstrijkje);
					}
					break;
				case ZAS:
					var zas = (CervixZas) HibernateHelper.deproxy(monster);
					hibernateService.delete(zas);
					break;
				}
			}
			var uitnodigingBrief = uitnodiging.getBrief();
			hibernateService.delete(uitnodiging);
			hibernateService.delete(uitnodigingBrief);
		}

		var brieven = ronde.getBrieven();
		for (var brief : brieven)
		{
			hibernateService.delete(brief);
		}

		var berichten = ronde.getHuisartsBerichten();
		for (var bericht : berichten)
		{
			hibernateService.delete(bericht);
		}

		var verslagen = ronde.getVerslagen();
		if (isNotEmpty(verslagen))
		{
			hibernateService.deleteAll(verslagen);
		}
		hibernateService.delete(ronde);
	}

	@Override
	public boolean heeftUitnodigingMetMonsterInLabproces(CervixScreeningRonde ronde)
	{

		for (var uitnodiging : ronde.getUitnodigingen())
		{
			var monster = uitnodiging.getMonster();
			if (monster != null && monster.getBrief() == null)
			{
				switch (uitnodiging.getMonsterType())
				{
				case UITSTRIJKJE:
					var uitstrijkje = (CervixUitstrijkje) HibernateHelper.deproxy(monster);
					var labformulier = uitstrijkje.getLabformulier();
					if (uitstrijkje.getUitstrijkjeStatus() != CervixUitstrijkjeStatus.NIET_ONTVANGEN
						|| labformulier != null && (labformulier.getStatus() == CervixLabformulierStatus.GECONTROLEERD
						|| labformulier.getStatus() == CervixLabformulierStatus.GECONTROLEERD_CYTOLOGIE))
					{
						return true; 
					}
					break;
				case ZAS:
					if (((CervixZas) HibernateHelper.deproxy(monster)).getZasStatus() != CervixZasStatus.VERSTUURD)
					{
						return true; 
					}
					break;
				}
			}
		}
		return false;
	}

	@Override
	public boolean heeftValideScreeningRondeVoorDigitaalLabformulier(CervixMonster monster)
	{
		var cervixVervolg = vervolgService.bepaalVervolg(monster, null, true).getVervolgTekst();
		return cervixVervolg == CervixVervolgTekst.UITSTRIJKJE_REGISTREER_ONTVANGST
			|| cervixVervolg == CervixVervolgTekst.UITSTRIJKJE_ONTVANGEN_NAAR_HPV
			|| cervixVervolg == CervixVervolgTekst.UITSTRIJKJE_GEANALYSEERD_OP_HPV_POGING_1_ONGELDIG_NAAR_HPV
			|| cervixVervolg == CervixVervolgTekst.UITSTRIJKJE_GEANALYSEERD_OP_HPV_POGING_2_ONGELDIG_VERNIETIG
			|| cervixVervolg == CervixVervolgTekst.UITSTRIJKJE_HPV_POSITIEF_NAAR_CYTOLOGIE
			|| cervixVervolg == CervixVervolgTekst.UITSTRIJKJE_REEDS_HPV_UITSLAG_NAAR_CYTOLOGIE
			|| cervixVervolg == CervixVervolgTekst.UITSTRIJKJE_VERVOLGONDERZOEK_NAAR_CYTOLOGIE
			|| cervixVervolg == CervixVervolgTekst.UITSTRIJKJE_NIET_ANALYSEERBAAR
			|| cervixVervolg == CervixVervolgTekst.UITSTRIJKJE_HPV_NEGATIEF_BEWAAR
			|| cervixVervolg == CervixVervolgTekst.UITSTRIJKJE_CYTOLOGIE_UITSLAG_BEWAAR
			|| cervixVervolg == CervixVervolgTekst.UITSTRIJKJE_HPV_POSITIEF_CYTOLOGIE_ONBEOORDEELBAAR_BEWAAR
			|| cervixVervolg == CervixVervolgTekst.UITSTRIJKJE_CLIENT_REEDS_GEINFORMEERD_BEWAAR
			|| cervixVervolg == CervixVervolgTekst.UITSTRIJKJE_CLIENT_REEDS_GEINFORMEERD_VERNIETIG;
	}

	@Override
	public Optional<CervixScreeningRonde> getLaatsteScreeningRonde(String bsn)
	{
		return screeningRondeRepository.findFirst(CervixScreeningRondeSpecification.heeftPersoon(bsn),
			Sort.by(Sort.Order.desc(ScreeningRonde_.CREATIE_DATUM)));
	}

	@Override
	public boolean heeftMaxAantalZASsenBereikt(CervixScreeningRonde laatsteScreeningRonde, boolean aangevraagdDoorClient)
	{
		var maxAantalZASAanvragen = getMaxAantalZASAanvragen(aangevraagdDoorClient);
		return screeningRondeRepository.count(CervixScreeningRondeSpecification.getZASsenHandmatigAangevraagdSpecification(laatsteScreeningRonde, aangevraagdDoorClient))
			>= maxAantalZASAanvragen;
	}

	@Override
	public Integer getMaxAantalZASAanvragen(boolean aangevraagdDoorClient)
	{
		Integer maxAantalZASaanvragen;
		if (aangevraagdDoorClient)
		{
			maxAantalZASaanvragen = preferenceService.getInteger(PreferenceKey.CERVIX_MAX_ZAS_AANVRAGEN_CLIENT.name());
		}
		else
		{
			maxAantalZASaanvragen = preferenceService.getInteger(PreferenceKey.CERVIX_MAX_ZAS_AANVRAGEN_INFOLIJN.name());
		}
		return maxAantalZASaanvragen;
	}

	@Override
	public boolean nieuweUitnodigingVoorClientMoetPUZijn(CervixScreeningRonde screeningRonde)
	{
		return bmhk2023StartBepalingService.rondeValtBinnenBmhk2023(screeningRonde) && !clientHeeftAanOnderzoekMeegedaanInRonde(screeningRonde);
	}

	@Override
	public boolean clientHeeftAanOnderzoekMeegedaanInRonde(CervixScreeningRonde screeningRonde)
	{
		return screeningRonde.getUitnodigingen().stream().anyMatch(uitnodiging -> uitnodiging.getMonster() != null && uitnodiging.getMonster().getOntvangstdatum() != null);
	}

	@Override
	public CervixScreeningRonde getOntvangstRondeVoorMonster(CervixMonster monster)
	{
		var peilmoment = monster.getOntvangstdatum();
		if (monster instanceof CervixUitstrijkje)
		{
			var labformulier = ((CervixUitstrijkje) monster).getLabformulier();
			if (labformulier != null
				&& (peilmoment == null || DateUtil.compareBefore(labformulier.getScanDatum(), peilmoment))
				&& (labformulier.getStatus() == CervixLabformulierStatus.GECONTROLEERD
				|| labformulier.getStatus() == CervixLabformulierStatus.GECONTROLEERD_CYTOLOGIE
				|| labformulier.getStatus() == CervixLabformulierStatus.HUISARTS_ONBEKEND))
			{
				peilmoment = labformulier.getScanDatum();
			}
		}
		if (peilmoment != null)
		{
			return screeningRondeRepository.findFirst(
					heeftMonsterInDossier(monster).and(isAangemaaktVoor(DateUtil.toLocalDateTime(peilmoment))),
					Sort.by(AbstractHibernateObject_.ID).descending())
				.orElseThrow(() -> new IllegalStateException("nog.geen.ronde.op.peildatum"));
		}
		return null;
	}

}
