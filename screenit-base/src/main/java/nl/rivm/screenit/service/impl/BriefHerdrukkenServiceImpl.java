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

import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.ClientBrief;
import nl.rivm.screenit.model.ScreeningRonde;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.algemeen.BezwaarBrief;
import nl.rivm.screenit.model.cervix.CervixAfmelding;
import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.enums.CervixMonsterType;
import nl.rivm.screenit.model.colon.ColonAfmelding;
import nl.rivm.screenit.model.colon.ColonBrief;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.mamma.MammaAfmelding;
import nl.rivm.screenit.model.mamma.MammaBrief;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.project.ProjectBrief;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.BriefHerdrukkenService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.cervix.CervixFactory;
import nl.rivm.screenit.util.BriefUtil;
import nl.rivm.screenit.util.EnumStringUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class BriefHerdrukkenServiceImpl implements BriefHerdrukkenService
{
	private static final Logger LOG = LoggerFactory.getLogger(BriefHerdrukkenServiceImpl.class);

	@Autowired
	private LogService logService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private BaseBriefService baseBriefService;

	@Autowired
	private CervixFactory cervixFactory;

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void opnieuwAanmaken(ClientBrief<?, ?, ?> brief, Account gebruiker)
	{
		brief = (ClientBrief<?, ?, ?>) HibernateHelper.deproxy(brief);
		Class<? extends ClientBrief> briefClass = brief.getClass();
		LOG.info("Kopieer brief " + brief.getBriefType() + " (clientId: " + brief.getClient().getId() + ")");
		if (ProjectBrief.class.equals(briefClass))
		{
			opnieuwAanmakenProjectBrief((ProjectBrief) brief);
		}
		else if (ColonBrief.class.equals(briefClass))
		{
			opnieuwAanmakenColonBrief((ColonBrief) brief);
		}
		else if (CervixBrief.class.equals(briefClass))
		{
			opnieuwAanmakenCervixBrief((CervixBrief) brief);
		}
		else if (MammaBrief.class.equals(briefClass))
		{
			opnieuwAanmakenMammaBrief((MammaBrief) brief);
		}
		else if (BezwaarBrief.class.equals(briefClass))
		{
			opnieuwAanmakenBezwaarbrief(brief);
		}

		BriefType type = brief.getBriefType();
		if (type == null)
		{
			LOG.warn("Geen brieftype bekend, betreft een projectbrief? briefID: " + brief.getId());
			logService.logGebeurtenis(LogGebeurtenis.BRIEF_HERDRUK, gebruiker, brief.getClient(), "Projectbrief");
		}
		else
		{
			logService.logGebeurtenis(LogGebeurtenis.BRIEF_HERDRUK, gebruiker, brief.getClient(), EnumStringUtil.getPropertyString(type), type.getOnderzoeken());
		}
	}

	private void opnieuwAanmakenBezwaarbrief(ClientBrief<?, ?, ?> brief)
	{
		BezwaarBrief oudeBrief = (BezwaarBrief) brief;
		BriefType briefType = oudeBrief.getBriefType();
		baseBriefService.checkVoorDubbeleBrieven(briefType, brief.getClient(), BezwaarBrief.class);

		BezwaarBrief nieuweBrief = new BezwaarBrief();
		nieuweBrief.setTemplateNaam(oudeBrief.getTemplateNaam());
		nieuweBrief.setMergedBrieven(null);
		nieuweBrief.setClient(brief.getClient());
		nieuweBrief.setGegenereerd(false);
		nieuweBrief.setHerdruk(oudeBrief);
		nieuweBrief.setCreatieDatum(currentDateSupplier.getDate());
		nieuweBrief.setBriefType(brief.getBriefType());
		nieuweBrief.setBezwaarMoment(oudeBrief.getBezwaarMoment());
		hibernateService.saveOrUpdate(nieuweBrief);
	}

	private void opnieuwAanmakenProjectBrief(ProjectBrief oudeProjectBrief)
	{
		ClientBrief<?, ?, ?> oudeBrief = (ClientBrief<?, ?, ?>) BriefUtil.getOrigineleBrief(oudeProjectBrief);

		ProjectBrief nieuweProjectBrief = new ProjectBrief();
		nieuweProjectBrief.setDefinitie(oudeProjectBrief.getDefinitie());
		nieuweProjectBrief.setProjectClient(oudeProjectBrief.getProjectClient());
		nieuweProjectBrief.setTeHerinnerenBrief(oudeProjectBrief.getTeHerinnerenBrief());
		nieuweProjectBrief.setTemplateNaam(oudeProjectBrief.getTemplateNaam());
		nieuweProjectBrief.setClient(oudeProjectBrief.getClient());
		nieuweProjectBrief.setGegenereerd(false);
		nieuweProjectBrief.setHerdruk(oudeProjectBrief);
		nieuweProjectBrief.setCreatieDatum(currentDateSupplier.getDate());
		nieuweProjectBrief.setBriefType(oudeProjectBrief.getBriefType());

		if (oudeBrief instanceof CervixBrief)
		{
			nieuweProjectBrief.setBrief(opnieuwAanmakenCervixBrief((CervixBrief) oudeBrief));
		}
		else if (oudeBrief instanceof ColonBrief)
		{
			nieuweProjectBrief.setBrief(opnieuwAanmakenColonBrief((ColonBrief) oudeBrief));
		}
		else if (oudeBrief instanceof MammaBrief)
		{
			nieuweProjectBrief.setBrief(opnieuwAanmakenMammaBrief((MammaBrief) oudeBrief));
		}

		hibernateService.saveOrUpdate(nieuweProjectBrief);
	}

	private MammaBrief opnieuwAanmakenMammaBrief(MammaBrief bestaandeBrief)
	{
		MammaAfmelding afmelding = bestaandeBrief.getAfmelding();

		MammaBrief nieuweBrief = afmelding != null ? baseBriefService.maakBvoBrief(afmelding, bestaandeBrief.getBriefType(), null) :
			baseBriefService.maakBvoBrief(bestaandeBrief.getScreeningRonde(), bestaandeBrief.getBriefType(), null);

		nieuweBrief.setUitnodiging(bestaandeBrief.getUitnodiging());
		nieuweBrief.setTemplateNaam(bestaandeBrief.getTemplateNaam());
		nieuweBrief.setHerdruk(bestaandeBrief);

		hibernateService.saveOrUpdate(nieuweBrief);
		return nieuweBrief;
	}

	private ColonBrief opnieuwAanmakenColonBrief(ColonBrief bestaandeBrief)
	{
		BriefType briefType = bestaandeBrief.getBriefType();
		ColonScreeningRonde screeningRonde = bestaandeBrief.getScreeningRonde();

		if (screeningRonde == null && briefType == BriefType.COLON_VOORAANKONDIGING)
		{

			for (ColonScreeningRonde ronde : bestaandeBrief.getClient().getColonDossier().getScreeningRondes())
			{
				if (screeningRonde == null || ronde.getId() < screeningRonde.getId())
				{
					screeningRonde = ronde;
				}
			}
		}

		ColonAfmelding afmelding = bestaandeBrief.getAfmelding();

		ColonBrief nieuweBrief = afmelding != null ? baseBriefService.maakBvoBrief(afmelding, briefType, null)
			: baseBriefService.maakBvoBrief(screeningRonde, briefType);

		nieuweBrief.setIntakeAfspraak(bestaandeBrief.getIntakeAfspraak());
		nieuweBrief.setVorigeIntakeAfspraak(bestaandeBrief.getVorigeIntakeAfspraak());
		nieuweBrief.setTemplateNaam(bestaandeBrief.getTemplateNaam());
		nieuweBrief.setHerdruk(bestaandeBrief);
		hibernateService.saveOrUpdate(nieuweBrief);

		return nieuweBrief;
	}

	private CervixBrief opnieuwAanmakenCervixBrief(CervixBrief bestaandeBrief)
	{
		CervixAfmelding afmelding = bestaandeBrief.getAfmelding();
		CervixScreeningRonde screeningRonde = bestaandeBrief.getScreeningRonde();
		CervixBrief nieuwBrief = afmelding != null ? baseBriefService.maakBvoBrief(afmelding, bestaandeBrief.getBriefType(), null) :
			baseBriefService.maakBvoBrief(screeningRonde, bestaandeBrief.getBriefType());

		nieuwBrief.setTemplateNaam(bestaandeBrief.getTemplateNaam());
		nieuwBrief.setHerdruk(bestaandeBrief);
		nieuwBrief.setAangevraagdeHerdruk(bestaandeBrief.isAangevraagdeHerdruk());
		hibernateService.saveOrUpdate(nieuwBrief);
		if (CervixMonsterType.getMonsterType(bestaandeBrief.getBriefType()) == CervixMonsterType.UITSTRIJKJE)
		{
			cervixFactory.maakUitnodiging(screeningRonde, nieuwBrief);
		}
		return nieuwBrief;
	}

	@Override
	public boolean magHerdrukken(ClientBrief<?, ?, ?> brief)
	{
		ScreeningRonde<?, ?, ?, ?> ronde = brief.getScreeningRonde();
		if (ronde != null && !ronde.equals(ronde.getDossier().getLaatsteScreeningRonde()))
		{
			return false;
		}

		ClientBrief<?, ?, ?> origineleBrief = (ClientBrief<?, ?, ?>) BriefUtil.getOrigineleBrief(brief);
		boolean magHerdrukken = true;
		if (origineleBrief instanceof CervixBrief)
		{
			CervixBrief cervixBrief = (CervixBrief) origineleBrief;
			CervixScreeningRonde screeningRonde = cervixBrief.getScreeningRonde();
			if (screeningRonde.getStatus() == ScreeningRondeStatus.AFGEROND && cervixBrief.getUitnodiging() != null)
			{
				magHerdrukken = false;
			}
			if (magHerdrukken)
			{
				magHerdrukken = !baseBriefService.briefTypeWachtOpKlaarzettenInDezeRonde(cervixBrief);
			}
		}
		else if (origineleBrief instanceof MammaBrief)
		{
			MammaBrief mammaBrief = (MammaBrief) origineleBrief;
			MammaScreeningRonde screeningRonde = mammaBrief.getScreeningRonde();
			if (mammaBrief.getUitnodiging() != null && screeningRonde.getStatus() == ScreeningRondeStatus.AFGEROND)
			{
				magHerdrukken = false;
			}
			if (magHerdrukken)
			{
				magHerdrukken = !baseBriefService.briefTypeWachtOpKlaarzettenInDezeRonde(mammaBrief);
			}
		}
		else if (origineleBrief instanceof ColonBrief)
		{
			magHerdrukken = BriefUtil.isGegenereerd(brief);
		}

		magHerdrukken &= !BriefUtil.isHerdruk(brief);

		return magHerdrukken;
	}
}
