package nl.rivm.screenit.service.colon.impl;

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

import java.util.List;

import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.AfmeldingType;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.colon.ColonAfmelding;
import nl.rivm.screenit.model.colon.ColonBrief;
import nl.rivm.screenit.model.colon.ColonConclusie;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.model.colon.OpenUitnodiging;
import nl.rivm.screenit.model.colon.enums.ColonAfmeldingReden;
import nl.rivm.screenit.model.colon.enums.ColonConclusieType;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingCategorie;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingsintervalType;
import nl.rivm.screenit.model.colon.enums.IFOBTTestStatus;
import nl.rivm.screenit.model.colon.enums.RedenAfspraakAfzeggen;
import nl.rivm.screenit.model.colon.planning.AfspraakStatus;
import nl.rivm.screenit.model.colon.planning.RoosterItem;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.OpenUitnodigingUitslag;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.OpenUitnodigingService;
import nl.rivm.screenit.service.colon.AfspraakService;
import nl.rivm.screenit.service.colon.ColonAfmeldService;
import nl.rivm.screenit.service.colon.ColonDossierBaseService;
import nl.rivm.screenit.service.colon.ColonScreeningsrondeService;
import nl.rivm.screenit.util.IFOBTTestUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang.BooleanUtils;
import org.joda.time.DateTime;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Component
@Transactional(propagation = Propagation.REQUIRED)
public class ColonAfmeldServiceImpl implements ColonAfmeldService
{
	private static final Logger LOG = LoggerFactory.getLogger(ColonAfmeldServiceImpl.class);

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private OpenUitnodigingService openUitnodigingService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private BaseBriefService briefService;

	@Autowired
	private ColonDossierBaseService dossierBaseService;

	@Autowired
	private AfspraakService afspraakService;

	@Autowired
	private ColonScreeningsrondeService screeningsrondeService;

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public ColonAfmelding maakAfmelding()
	{
		return new ColonAfmelding();
	}

	@Override
	public void definitieveAfmeldingAanvragen(ColonAfmelding afmelding, boolean rappelBrief)
	{
		BriefType briefType = BriefType.COLON_AFMELDING_AANVRAAG;
		if (rappelBrief)
		{
			briefType = BriefType.COLON_AFMELDING_HANDTEKENING;
		}
		afmelding.setAfmeldingAanvraag(briefService.maakColonBrief(afmelding, briefType, currentDateSupplier.getDate()));
		hibernateService.saveOrUpdate(afmelding);
	}

	@Override
	public void eenmaligAfmelden(ColonAfmelding afmelding, Account account)
	{
		ColonScreeningRonde ronde = afmelding.getScreeningRonde();

		afspraakAfzeggen(ronde);
		nietVerzondenUitnodigingenVerwijderen(ronde);
		openUitnodigingService.afmeldingHeraanmeldingReactieOpOpenUitnodiging(afmelding, ronde, account);
	}

	private void afspraakAfzeggen(ColonScreeningRonde ronde)
	{
		ColonIntakeAfspraak laatsteAfspraak = ronde.getLaatsteAfspraak();
		if (laatsteAfspraak != null)
		{
			ColonConclusieType conclusieType = null;
			ColonConclusie conclusie = laatsteAfspraak.getConclusie();
			if (conclusie != null)
			{
				conclusieType = conclusie.getType();
			}
			AfspraakStatus status = laatsteAfspraak.getStatus();
			if (AfspraakStatus.GEPLAND.equals(status)
				|| AfspraakStatus.UITGEVOERD.equals(status) && ColonConclusieType.NO_SHOW.equals(conclusieType))
			{
				laatsteAfspraak.setRedenAfzeggen(RedenAfspraakAfzeggen.CLIENT_WIL_NIET_DEELNEMEN);
				afspraakService.afspraakAfzeggen(laatsteAfspraak, AfspraakStatus.GEANNULEERD_AFMELDEN, currentDateSupplier.getLocalDateTime(), false);

				LOG.info("Afmelding: laatste intake afspraak is afgezegd");
			}
		}
	}

	private void nietVerzondenUitnodigingenVerwijderen(ColonScreeningRonde ronde)
	{
		ColonUitnodiging laatsteUitnodiging = ronde.getLaatsteUitnodiging();
		if (laatsteUitnodiging != null && !laatsteUitnodiging.isVerstuurd())
		{
			List<ColonUitnodiging> uitnodigingen = ronde.getUitnodigingen();
			uitnodigingen.remove(laatsteUitnodiging);

			ColonUitnodiging nieuweLaatsteUitnodiging = null;
			for (ColonUitnodiging uitnodiging : uitnodigingen)
			{
				if (nieuweLaatsteUitnodiging == null || nieuweLaatsteUitnodiging.getUitnodigingsId() < uitnodiging.getUitnodigingsId())
				{
					nieuweLaatsteUitnodiging = uitnodiging;
				}
			}
			ronde.setLaatsteUitnodiging(nieuweLaatsteUitnodiging);
			hibernateService.delete(laatsteUitnodiging);
		}
	}

	@Override
	public void vervolgAfmelden(ColonAfmelding afmelding)
	{
		if (afmelding.getType() == AfmeldingType.DEFINITIEF)
		{
			if (!ColonAfmeldingReden.PROEF_BEVOLKINGSONDERZOEK.equals(afmelding.getReden()))
			{
				afmelding.setAfmeldingBevestiging(
					briefService.maakColonBrief(afmelding, BriefType.COLON_AFMELDING_BEVESTIGING, currentDateSupplier.getDate()));
				hibernateService.saveOrUpdate(afmelding);
			}

			ColonScreeningRonde ronde = afmelding.getDossier().getLaatsteScreeningRonde();
			if (ronde != null && ronde.getOpenUitnodiging() != null && ronde.getOpenUitnodiging().getUitslag() == null)
			{
				OpenUitnodiging uitnodiging = ronde.getOpenUitnodiging();
				uitnodiging.setUitslag(OpenUitnodigingUitslag.AFMELDING);
				uitnodiging.setAfmelding(afmelding);
				hibernateService.saveOrUpdate(uitnodiging);
			}
		}
	}

	@Override
	public void vervolgHeraanmelden(ColonAfmelding herAanTeMeldenAfmelding, Account account)
	{
		ColonScreeningRonde ronde = getGeldigeRondeVoorHeraanmelding(herAanTeMeldenAfmelding);

		DateTime nu = currentDateSupplier.getDateTime();
		if (herAanTeMeldenAfmelding.getType() == AfmeldingType.DEFINITIEF)
		{
			if (!ColonAfmeldingReden.PROEF_BEVOLKINGSONDERZOEK.equals(herAanTeMeldenAfmelding.getReden())
				&& BooleanUtils.isNotTrue(herAanTeMeldenAfmelding.getHeraanmeldingBevestigingsBriefTegenhouden()))
			{
				herAanTeMeldenAfmelding.setHeraanmeldBevestiging(
					briefService.maakColonBrief(herAanTeMeldenAfmelding, BriefType.COLON_HERAANMELDING_BEVESTIGING, nu.toDate()));
				hibernateService.saveOrUpdate(herAanTeMeldenAfmelding);
			}
		}

		ColonIntakeAfspraak afspraak = herAanTeMeldenAfmelding.getHeraanmeldingAfspraak();
		if (herAanTeMeldenAfmelding.getClientWilNieuweUitnodiging() && afspraak == null)
		{
			screeningsrondeService.createNieuweUitnodiging(ronde, ColonUitnodigingCategorie.U4_2);
		}

		if (afspraak != null)
		{
			Client client = ronde.getDossier().getClient();
			afspraak.setClient(client);
			afspraak.setColonScreeningRonde(ronde);
			afspraak.setDatumLaatsteWijziging(nu.plusMillis(100).toDate());

			RoosterItem roosterItem = null;
			if (Boolean.TRUE.equals(herAanTeMeldenAfmelding.getHeraanmeldingAfspraakUitRooster()))
			{
				roosterItem = afspraakService.getRoosterBlokVoorAfspraak(afspraak);
				afspraak.setRoosterItem(roosterItem);
			}
			hibernateService.saveOrUpdate(afspraak);
			if (roosterItem != null)
			{
				roosterItem.getAfspraken().add(afspraak);
				hibernateService.saveOrUpdate(roosterItem);
			}

			ronde.getAfspraken().add(afspraak);
			ronde.setLaatsteAfspraak(afspraak);
			client.getAfspraken().add(afspraak);
			hibernateService.saveOrUpdate(client);
			dossierBaseService.setDatumVolgendeUitnodiging(ronde.getDossier(), ColonUitnodigingsintervalType.GEPLANDE_INTAKE_AFSPRAAK);

			BriefType afspraakBriefType = herAanTeMeldenAfmelding.getHeraanmeldingAfspraakBriefType();
			if (afspraakBriefType == null)
			{
				afspraakBriefType = BriefType.COLON_UITNODIGING_INTAKE;
			}
			ColonBrief brief = briefService.maakColonBrief(ronde, afspraakBriefType, nu.plusMillis(150).toDate());

			if (herAanTeMeldenAfmelding.getHeraanmeldingAfspraakBriefTegenhouden())
			{
				brief.setTegenhouden(true);
			}
			brief.setIntakeAfspraak(afspraak);
			hibernateService.saveOrUpdate(brief);
			LOG.info("Heraanmelden: nieuw afspraak aangemaakt");
		}

		if (ronde != null)
		{
			openUitnodigingService.afmeldingHeraanmeldingReactieOpOpenUitnodiging(herAanTeMeldenAfmelding, ronde, account);
			if (!herAanTeMeldenAfmelding.getClientWilNieuweUitnodiging())
			{
				ColonUitnodiging uitnodiging = ronde.getLaatsteUitnodiging();
				if (uitnodiging != null)
				{
					IFOBTTest gekoppeldeTest = uitnodiging.getGekoppeldeTest();
					IFOBTTestStatus ifobtTestStatus = IFOBTTestUtil.getActieveIFOBTTestStatusNaHeraanmelding(uitnodiging);
					if (ifobtTestStatus != null && gekoppeldeTest != null
						&& gekoppeldeTest.getStatus().magWijzigenNaarStatus(ifobtTestStatus, gekoppeldeTest))
					{
						gekoppeldeTest.setStatus(ifobtTestStatus);
						gekoppeldeTest.setStatusDatum(nu.plusMillis(100).toDate());
						hibernateService.saveOrUpdate(gekoppeldeTest);
					}
				}
			}
		}
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public ColonScreeningRonde getGeldigeRondeVoorHeraanmelding(ColonAfmelding herAanTeMeldenAfmelding)
	{
		ColonScreeningRonde ronde = null;
		switch (herAanTeMeldenAfmelding.getType())
		{
		case EENMALIG:
			if (Boolean.TRUE.equals(herAanTeMeldenAfmelding.getRondeGesloten()))
			{
				ronde = herAanTeMeldenAfmelding.getScreeningRonde();
			}
			break;
		case DEFINITIEF:
			ronde = herAanTeMeldenAfmelding.getDossier().getLaatsteScreeningRonde();
			break;
		default:
			throw new IllegalStateException();
		}

		return ronde;
	}

}
