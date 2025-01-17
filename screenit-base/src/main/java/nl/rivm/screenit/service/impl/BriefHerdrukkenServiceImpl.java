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

import java.util.ArrayList;
import java.util.List;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Afmelding;
import nl.rivm.screenit.model.ClientBrief;
import nl.rivm.screenit.model.ScreeningRonde;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.algemeen.AlgemeneBrief;
import nl.rivm.screenit.model.algemeen.BezwaarBrief;
import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.enums.CervixMonsterType;
import nl.rivm.screenit.model.colon.ColonBrief;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.mamma.MammaBrief;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.project.ProjectBrief;
import nl.rivm.screenit.model.project.ProjectBriefActieType;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.BriefHerdrukkenService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.cervix.CervixFactory;
import nl.rivm.screenit.util.BriefUtil;
import nl.rivm.screenit.util.EnumStringUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Slf4j
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class BriefHerdrukkenServiceImpl implements BriefHerdrukkenService
{
	@Autowired
	private LogService logService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private BaseBriefService briefService;

	@Autowired(required = false)
	private CervixFactory cervixFactory;

	@Override
	@Transactional
	public void opnieuwAanmaken(List<? extends ClientBrief<?, ?, ?>> brieven, Account gebruiker)
	{
		brieven.forEach(brief -> opnieuwAanmaken(brief, gebruiker));
	}

	@Override
	@Transactional
	public void opnieuwAanmaken(ClientBrief<?, ?, ?> brief, Account gebruiker)
	{
		brief = (ClientBrief<?, ?, ?>) HibernateHelper.deproxy(brief);
		Class<? extends ClientBrief> briefClass = brief.getClass();
		LOG.info("Kopieer brief {} (clientId: {})", brief.getBriefType(), brief.getClient().getId());
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
		else if (AlgemeneBrief.class.equals(briefClass))
		{
			opnieuwAanmakenAlgemenebrief((AlgemeneBrief) brief);
		}

		BriefType type = brief.getBriefType();
		if (type == null)
		{
			LOG.warn("Geen brieftype bekend, betreft een projectbrief? briefID: {}", brief.getId());
			logService.logGebeurtenis(LogGebeurtenis.BRIEF_HERDRUK, gebruiker, brief.getClient(), "Projectbrief");
		}
		else
		{
			logService.logGebeurtenis(LogGebeurtenis.BRIEF_HERDRUK, gebruiker, brief.getClient(), EnumStringUtil.getPropertyString(type), type.getOnderzoeken());
		}
	}

	private void opnieuwAanmakenAlgemenebrief(AlgemeneBrief oudeBrief)
	{
		var nieuweBrief = briefService.maakAlgemeneBrief(oudeBrief.getClient(), oudeBrief.getBriefType());
		nieuweBrief.setBriefDefinitie(oudeBrief.getBriefDefinitie());
		nieuweBrief.setTemplateNaam(oudeBrief.getTemplateNaam());
		nieuweBrief.setHerdruk(oudeBrief);
		hibernateService.saveOrUpdate(nieuweBrief);
	}

	private void opnieuwAanmakenBezwaarbrief(ClientBrief<?, ?, ?> brief)
	{
		BezwaarBrief oudeBrief = (BezwaarBrief) brief;
		BezwaarBrief nieuweBrief = new BezwaarBrief();
		nieuweBrief.setBriefDefinitie(oudeBrief.getBriefDefinitie());
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

		ClientBrief nieuweBrief = null;
		if (oudeBrief instanceof CervixBrief)
		{
			nieuweBrief = opnieuwAanmakenCervixBrief((CervixBrief) oudeBrief);
		}
		else if (oudeBrief instanceof ColonBrief)
		{
			nieuweBrief = opnieuwAanmakenColonBrief((ColonBrief) oudeBrief);
		}
		else if (oudeBrief instanceof MammaBrief)
		{
			nieuweBrief = opnieuwAanmakenMammaBrief((MammaBrief) oudeBrief);
		}
		if (nieuweBrief != null)
		{
			nieuweBrief.setProjectBrief(nieuweProjectBrief);
			nieuweBrief.setVervangendeProjectBrief(true);
			nieuweProjectBrief.setBrief(nieuweBrief);
		}

		hibernateService.saveOrUpdate(nieuweProjectBrief);
	}

	private MammaBrief opnieuwAanmakenMammaBrief(MammaBrief bestaandeBrief)
	{
		MammaBrief nieuweBrief = opnieuwAanmakenClientBrief(bestaandeBrief);
		nieuweBrief.setUitnodiging(bestaandeBrief.getUitnodiging());
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

		ColonBrief nieuweBrief = opnieuwAanmakenClientBrief(bestaandeBrief);
		nieuweBrief.setIntakeAfspraak(bestaandeBrief.getIntakeAfspraak());
		nieuweBrief.setVorigeIntakeAfspraak(bestaandeBrief.getVorigeIntakeAfspraak());
		hibernateService.saveOrUpdate(nieuweBrief);
		return nieuweBrief;
	}

	private CervixBrief opnieuwAanmakenCervixBrief(CervixBrief bestaandeBrief)
	{
		CervixBrief nieuweBrief = opnieuwAanmakenClientBrief(bestaandeBrief);
		nieuweBrief.setAangevraagdeHerdruk(bestaandeBrief.isAangevraagdeHerdruk());
		hibernateService.saveOrUpdate(nieuweBrief);
		if (CervixMonsterType.getMonsterType(bestaandeBrief.getBriefType()) == CervixMonsterType.UITSTRIJKJE)
		{
			cervixFactory.maakHeraanvraagUitnodiging(bestaandeBrief.getScreeningRonde(), nieuweBrief);
		}
		return nieuweBrief;
	}

	private <B extends ClientBrief<S, A, B>, A extends Afmelding<S, ?, B>, S extends ScreeningRonde<?, B, A, ?>> B opnieuwAanmakenClientBrief(B bestaandeBrief)
	{
		A afmelding = bestaandeBrief.getAfmelding();
		S screeningRonde = bestaandeBrief.getScreeningRonde();
		BriefType briefType = bestaandeBrief.getBriefType();
		B nieuweBrief = afmelding != null ? briefService.maakBvoBrief(afmelding, briefType, null, bestaandeBrief.isVervangendeProjectBrief()) :
			briefService.maakBvoBrief(screeningRonde, briefType, null, false, bestaandeBrief.isVervangendeProjectBrief());

		nieuweBrief.setTemplateNaam(bestaandeBrief.getTemplateNaam());
		nieuweBrief.setHerdruk(bestaandeBrief);
		return nieuweBrief;
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
			if (BriefType.getCervixUitstrijkjeBrieven().contains(cervixBrief.getBriefType())
				|| BriefType.getCervixZasBrieven().contains(cervixBrief.getBriefType()))
			{
				return false;
			}

			CervixScreeningRonde screeningRonde = cervixBrief.getScreeningRonde();
			if (screeningRonde.getStatus() == ScreeningRondeStatus.AFGEROND && cervixBrief.getUitnodiging() != null)
			{
				magHerdrukken = false;
			}
			if (magHerdrukken)
			{
				magHerdrukken = !briefService.briefTypeWachtOpKlaarzettenInDezeRonde(cervixBrief);
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
				magHerdrukken = !briefService.briefTypeWachtOpKlaarzettenInDezeRonde(mammaBrief);
			}
		}
		else if (origineleBrief instanceof ColonBrief)
		{
			magHerdrukken = BriefUtil.isGegenereerd(brief);
		}
		else if (origineleBrief instanceof ProjectBrief && brief instanceof ProjectBrief)
		{
			ProjectBrief projectBrief = (ProjectBrief) brief;
			var bvoOnafhankelijkeBriefActieTypes = new ArrayList<>(List.of(ProjectBriefActieType.values()));
			bvoOnafhankelijkeBriefActieTypes.remove(ProjectBriefActieType.VERVANGENDEBRIEF);
			if (bvoOnafhankelijkeBriefActieTypes.contains(projectBrief.getDefinitie().getType())
				&& projectBrief.getProjectClient().getBrieven().stream()
				.filter(b -> b.getDefinitie().equals(projectBrief.getDefinitie()))
				.anyMatch(BriefUtil::isNietGegenereerdEnNietVervangen))
			{
				magHerdrukken = false;
			}
		}
		else if (origineleBrief instanceof AlgemeneBrief)
		{
			AlgemeneBrief algemeneBrief = (AlgemeneBrief) origineleBrief;
			magHerdrukken = BriefUtil.isGegenereerd(brief);
			if (algemeneBrief.getBriefDefinitie() != null && algemeneBrief.getClient().getAlgemeneBrieven().stream()
				.filter(b -> b.getBriefDefinitie().equals(algemeneBrief.getBriefDefinitie()))
				.anyMatch(BriefUtil::isNietGegenereerdEnNietVervangen))
			{
				magHerdrukken = false;
			}
		}

		magHerdrukken &= !BriefUtil.isHerdruk(brief);

		return magHerdrukken;
	}
}
