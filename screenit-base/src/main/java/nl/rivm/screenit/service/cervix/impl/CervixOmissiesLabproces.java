package nl.rivm.screenit.service.cervix.impl;

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
import java.util.Date;
import java.util.List;

import javax.annotation.PostConstruct;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.model.cervix.CervixHuisartsBericht;
import nl.rivm.screenit.model.cervix.CervixLabformulier;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.enums.CervixHpvBeoordelingWaarde;
import nl.rivm.screenit.model.cervix.enums.CervixLabformulierStatus;
import nl.rivm.screenit.model.cervix.enums.CervixOmissieType;
import nl.rivm.screenit.model.cervix.enums.CervixUitstrijkjeStatus;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.HuisartsBerichtType;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.cervix.CervixHuisartsBerichtService;
import nl.rivm.screenit.service.cervix.CervixMailService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.cervix.CervixMonsterUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@Scope("prototype")
public class CervixOmissiesLabproces
{
	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private CervixHuisartsBerichtService huisartsBerichtService;

	@Autowired
	private CervixMailService cervixMailService;

	private final CervixMonster monster;

	private List<Omissie> omissieLijst;

	private CervixOmissiesLabproces(CervixMonster monster)
	{
		this.monster = monster;
	}

	@PostConstruct
	private void init()
	{
		omissieLijst = new ArrayList<>();
		omissieLijst.add(omissieWachtOpUitstrijkjeOntvangen());
		omissieLijst.add(omissieUitstrijkjeOntbreekt());
		omissieLijst.add(omissieWachtOpHpvUitslag());
		omissieLijst.add(omissieWachtOpGecontroleerd());
		omissieLijst.add(omissieLabformulierOntbreekt());
		omissieLijst.add(omissieWachtOpGecontroleerdVoorCytologie());
		omissieLijst.add(omissieWachtOpCytologieUitslag());
	}

	public Omissie bepaalOmissieEnVoerActieUit()
	{
		for (var omissie : omissieLijst)
		{
			if (omissie.vanToepassing() && omissie.getStartWachttijd() != null)
			{
				var preferenceKeyWachttijd = omissie.preferenceKeyWachttijd;

				var eindWachttijd = DateUtil.plusWerkdagen(omissie.getStartWachttijd(), preferenceService.getInteger(preferenceKeyWachttijd.name()));

				if (dateSupplier.getLocalDateTime().isAfter(DateUtil.toLocalDateTime(eindWachttijd)))
				{
					omissie.voerActieUit();
					LOG.info("Omissie " + preferenceKeyWachttijd.name() + " uitgevoerd voor client met id "
						+ monster.getOntvangstScreeningRonde().getDossier().getClient().getId());
					return omissie;
				}

				var preferenceKeyWachttijdWaarschuwing = omissie.preferenceKeyWachttijdWaarschuwing;
				if (preferenceKeyWachttijdWaarschuwing != null)
				{
					var eindWachttijdWaarschuwing = DateUtil.plusWerkdagen(omissie.getStartWachttijd(), preferenceService.getInteger(preferenceKeyWachttijdWaarschuwing.name()));
					if (dateSupplier.getLocalDateTime().isAfter(DateUtil.toLocalDateTime(eindWachttijdWaarschuwing)))
					{
						omissie.verstuurWaarschuwing();
						LOG.info("Gewaarschuwd voor omissie " + preferenceKeyWachttijd.name() + " voor client met id "
							+ monster.getOntvangstScreeningRonde().getDossier().getClient().getId());
					}
				}
			}
		}
		return null;
	}

	private Omissie omissieUitstrijkjeOntbreekt()
	{
		return new Omissie(PreferenceKey.CERVIX_WACHTTIJD_UITSTRIJKJE_ONTBREEKT_ANALOOG, CervixOmissieType.UITSTRIJKJE_ONTBREEKT)
		{

			private CervixUitstrijkje uitstrijkje;

			@Override
			protected boolean vanToepassing()
			{
				if (monster instanceof CervixUitstrijkje)
				{
					uitstrijkje = (CervixUitstrijkje) monster;
					if (labformulier != null)
					{
						if (uitstrijkje.getUitstrijkjeStatus() == CervixUitstrijkjeStatus.NIET_ONTVANGEN
							&& (labformulier.getStatus() == CervixLabformulierStatus.GECONTROLEERD
							|| labformulier.getStatus() == CervixLabformulierStatus.GECONTROLEERD_CYTOLOGIE)
							&& labformulier.getUitstrijkjeOntbreektHuisartsBericht() == null)
						{
							return true;
						}
					}
				}
				return false;
			}

			@Override
			protected Date getStartWachttijd()
			{
				return labformulier.getStatusDatum();
			}

			@Override
			protected void voerActieUit()
			{
				var huisartsBericht = huisartsBerichtService.maakCervixHuisartsBericht(
					HuisartsBerichtType.CERVIX_UITSTRIJKJE_ONTBREEKT,
					monster.getOntvangstScreeningRonde().getDossier().getClient(), labformulier.getHuisartsLocatie(),
					uitstrijkje.getUitnodiging());

				labformulier.setUitstrijkjeOntbreektHuisartsBericht(huisartsBericht);
				huisartsBericht.setLabformulier(labformulier);

				hibernateService.saveOrUpdateAll(huisartsBericht, labformulier);

				uitstrijkjeOntbreektHuisartsBericht = huisartsBericht;

			}

			@Override
			protected void verstuurWaarschuwing()
			{

			}

		};
	}

	private Omissie omissieWachtOpUitstrijkjeOntvangen()
	{
		return new Omissie(PreferenceKey.CERVIX_WACHTTIJD_WACHT_OP_UITSTRIJKJE_ONTVANGEN_ANALOOG, CervixOmissieType.WACHT_OP_UITSTRIJKJE_ONTVANGEN)
		{
			@Override
			protected boolean vanToepassing()
			{
				if (monster instanceof CervixUitstrijkje)
				{
					CervixUitstrijkje uitstrijkje = (CervixUitstrijkje) monster;
					if (labformulier != null)
					{
						if (uitstrijkje.getUitstrijkjeStatus() == CervixUitstrijkjeStatus.NIET_ONTVANGEN
							&& (labformulier.getStatus() == CervixLabformulierStatus.GECONTROLEERD
							|| labformulier.getStatus() == CervixLabformulierStatus.GECONTROLEERD_CYTOLOGIE
							|| labformulier.getStatus() == CervixLabformulierStatus.HUISARTS_ONBEKEND))
						{
							return true;
						}
					}
				}
				return false;
			}

			@Override
			protected Date getStartWachttijd()
			{
				return labformulier.getStatusDatum();
			}

			@Override
			protected void voerActieUit()
			{
				if (monster.getOntvangstScreeningRonde().getMonsterHpvUitslag() == null)
				{
					briefType = BriefType.CERVIX_UITSTRIJKJE_NIET_ANALYSEERBAAR_OF_HPV_ONBEOORDEELBAAR;
				}
				else
				{
					briefType = BriefType.CERVIX_UITSTRIJKJE_NIET_ANALYSEERBAAR_OF_CYTOLOGIE_ONBEOORDEELBAAR;
				}

				huisartsBerichtType = HuisartsBerichtType.CERVIX_UITSTRIJKJE_NIET_ANALYSEERBAAR;

			}

			@Override
			protected void verstuurWaarschuwing()
			{

			}
		};
	}

	private Omissie omissieWachtOpHpvUitslag()
	{
		return new Omissie(PreferenceKey.CERVIX_WACHTTIJD_WACHT_OP_HPV_UITSLAG, CervixOmissieType.WACHT_OP_HPV_UITSLAG)
		{
			@Override
			protected boolean vanToepassing()
			{
				return monster.getOntvangstScreeningRonde().getMonsterHpvUitslag() == null;
			}

			@Override
			protected Date getStartWachttijd()
			{
				return monster.getOntvangstdatum();
			}

			@Override
			protected void voerActieUit()
			{
				if (monster instanceof CervixUitstrijkje)
				{
					briefType = BriefType.CERVIX_UITSTRIJKJE_NIET_ANALYSEERBAAR_OF_HPV_ONBEOORDEELBAAR;
					huisartsBerichtType = HuisartsBerichtType.CERVIX_UITSTRIJKJE_NIET_ANALYSEERBAAR;
				}
				else
				{
					briefType = BriefType.CERVIX_ZAS_NIET_ANALYSEERBAAR_OF_ONBEOORDEELBAAR;
				}

			}

			@Override
			protected void verstuurWaarschuwing()
			{

			}

		};
	}

	private Omissie omissieLabformulierOntbreekt()
	{
		return new Omissie(PreferenceKey.CERVIX_WACHTTIJD_LABFORMULIER_ONTBREEKT, CervixOmissieType.LABFORMULIER_ONTBREEKT)
		{

			private CervixUitstrijkje uitstrijkje;

			@Override
			protected boolean vanToepassing()
			{
				var monsterHpvUitslag = monster.getOntvangstScreeningRonde().getMonsterHpvUitslag();
				if (monsterHpvUitslag != null && monsterHpvUitslag.getLaatsteHpvBeoordeling().getHpvUitslag() == CervixHpvBeoordelingWaarde.POSITIEF)
				{
					uitstrijkje = (CervixUitstrijkje) monster;

					if (labformulier == null || labformulier.getStatus() == CervixLabformulierStatus.GESCAND
						|| labformulier.getStatus() == CervixLabformulierStatus.AFGEKEURD && !labformulier.getKunstmatig())
					{
						return true;
					}
				}
				return false;
			}

			@Override
			protected Date getStartWachttijd()
			{
				return uitstrijkje.getOntvangstdatum();
			}

			@Override
			protected void voerActieUit()
			{
				var kunstmatigLabformulier = new CervixLabformulier();
				kunstmatigLabformulier.setKunstmatig(true);
				kunstmatigLabformulier.setScanDatum(dateSupplier.getDate());
				kunstmatigLabformulier.setBarcode(uitstrijkje.getMonsterId());
				kunstmatigLabformulier.setObjid("K" + uitstrijkje.getMonsterId()); 
				kunstmatigLabformulier.setLaboratorium(uitstrijkje.getLaboratorium());
				kunstmatigLabformulier.setStatus(CervixLabformulierStatus.HUISARTS_ONBEKEND);
				kunstmatigLabformulier.setStatusDatum(dateSupplier.getDate());

				if (labformulier != null)
				{
					labformulier.setUitstrijkje(uitstrijkje);
					hibernateService.saveOrUpdate(labformulier);
				}

				uitstrijkje.setLabformulier(kunstmatigLabformulier);
				kunstmatigLabformulier.setUitstrijkje(uitstrijkje);

				hibernateService.saveOrUpdateAll(kunstmatigLabformulier, uitstrijkje);

			}

			@Override
			protected void verstuurWaarschuwing()
			{

			}

		};
	}

	private Omissie omissieWachtOpGecontroleerd()
	{
		return new Omissie(PreferenceKey.CERVIX_WACHTTIJD_WACHT_OP_GECONTROLEERD, CervixOmissieType.WACHT_OP_GECONTROLEERD)
		{

			private CervixUitstrijkje uitstrijkje;

			@Override
			protected boolean vanToepassing()
			{
				var monsterHpvUitslag = monster.getOntvangstScreeningRonde().getMonsterHpvUitslag();
				if (monsterHpvUitslag != null && monsterHpvUitslag.getLaatsteHpvBeoordeling().getHpvUitslag() == CervixHpvBeoordelingWaarde.POSITIEF)
				{
					uitstrijkje = (CervixUitstrijkje) monster;

					if (labformulier == null || labformulier.getStatus() == CervixLabformulierStatus.GESCAND
						|| labformulier.getStatus() == CervixLabformulierStatus.AFGEKEURD
						|| labformulier.getStatus() == CervixLabformulierStatus.HUISARTS_ONBEKEND)
					{
						return true;
					}
				}
				return false;
			}

			@Override
			protected Date getStartWachttijd()
			{
				return uitstrijkje.getOntvangstdatum();
			}

			@Override
			protected void voerActieUit()
			{
				if (uitstrijkje.getLabformulier() != null && CervixLabformulierStatus.HUISARTS_ONBEKEND.equals(uitstrijkje.getLabformulier().getStatus()))
				{
					cervixMailService.sendWachttijdVerstrekenMetHuisartsOnbekend(uitstrijkje);
				}
				if (monster.getOntvangstScreeningRonde().getMonsterHpvUitslag().equals(monster))
				{
					briefType = BriefType.CERVIX_CYTOLOGIE_ONBEOORDEELBAAR;
				}
				else
				{
					briefType = BriefType.CERVIX_UITSTRIJKJE_NIET_ANALYSEERBAAR_OF_CYTOLOGIE_ONBEOORDEELBAAR;
				}

				huisartsBerichtType = HuisartsBerichtType.CERVIX_OMISSIE_NA_UITSTRIJKJE_HPV_POSITIEF;
			}

			@Override
			protected void verstuurWaarschuwing()
			{

			}
		};
	}

	private Omissie omissieWachtOpGecontroleerdVoorCytologie()
	{
		return new Omissie(PreferenceKey.CERVIX_WACHTTIJD_WACHT_OP_GECONTROLEERD_VOOR_CYTOLOGIE, CervixOmissieType.WACHT_OP_GECONTROLEERD_VOOR_CYTOLOGIE)
		{

			private CervixUitstrijkje uitstrijkje;

			@Override
			protected boolean vanToepassing()
			{
				var monsterHpvUitslag = monster.getOntvangstScreeningRonde().getMonsterHpvUitslag();
				if (monsterHpvUitslag != null && monsterHpvUitslag.getLaatsteHpvBeoordeling().getHpvUitslag() == CervixHpvBeoordelingWaarde.POSITIEF)
				{
					uitstrijkje = (CervixUitstrijkje) monster;
					if (labformulier != null && labformulier.getStatus() != CervixLabformulierStatus.GECONTROLEERD_CYTOLOGIE)
					{
						return true;
					}
				}
				return false;
			}

			@Override
			protected Date getStartWachttijd()
			{
				return uitstrijkje.getOntvangstdatum();
			}

			@Override
			protected void voerActieUit()
			{
				if (monster.getOntvangstScreeningRonde().getMonsterHpvUitslag().equals(monster))
				{
					briefType = BriefType.CERVIX_CYTOLOGIE_ONBEOORDEELBAAR;
				}
				else
				{
					briefType = BriefType.CERVIX_UITSTRIJKJE_NIET_ANALYSEERBAAR_OF_CYTOLOGIE_ONBEOORDEELBAAR;
				}
				huisartsBerichtType = HuisartsBerichtType.CERVIX_OMISSIE_NA_UITSTRIJKJE_HPV_POSITIEF;
			}

			@Override
			protected void verstuurWaarschuwing()
			{

			}
		};
	}

	public Omissie omissieWachtOpCytologieUitslag()
	{
		return new Omissie(PreferenceKey.CERVIX_WACHTTIJD_WACHT_OP_CYTOLOGIE_UITSLAG, PreferenceKey.CERVIX_WACHTTIJD_WACHT_OP_WAARSCHUWING_CYTOLOGIE_UITSLAG,
			CervixOmissieType.WACHT_OP_CYTOLOGIE_UITSLAG)
		{

			@Override
			protected boolean vanToepassing()
			{
				if (monster instanceof CervixUitstrijkje)
				{
					var uitstrijkje = CervixMonsterUtil.getUitstrijkje(monster);
					return uitstrijkje.getOntvangstdatum() != null && uitstrijkje.getCytologieOrder() != null;
				}
				return false;
			}

			@Override
			protected Date getStartWachttijd()
			{
				return monster.getOntvangstdatum();
			}

			@Override
			protected void voerActieUit()
			{
				if (monster.getOntvangstScreeningRonde().getMonsterHpvUitslag().equals(monster))
				{
					briefType = BriefType.CERVIX_CYTOLOGIE_ONBEOORDEELBAAR;
				}
				else
				{
					briefType = BriefType.CERVIX_UITSTRIJKJE_NIET_ANALYSEERBAAR_OF_CYTOLOGIE_ONBEOORDEELBAAR;
				}

				huisartsBerichtType = HuisartsBerichtType.CERVIX_OMISSIE_NA_UITSTRIJKJE_HPV_POSITIEF;

			}

			@Override
			protected void verstuurWaarschuwing()
			{
				cervixMailService.sendWachtOpCytologieUitslagMail(monster);
			}
		};
	}

	public abstract class Omissie
	{
		protected PreferenceKey preferenceKeyWachttijd;

		protected final PreferenceKey preferenceKeyWachttijdWaarschuwing;

		protected CervixLabformulier labformulier;

		protected BriefType briefType;

		protected HuisartsBerichtType huisartsBerichtType;

		protected CervixHuisartsBericht uitstrijkjeOntbreektHuisartsBericht;

		protected CervixOmissieType omissieType;

		protected Omissie(PreferenceKey preferenceKeyWachttijd, PreferenceKey preferenceKeyWachttijdWaarschuwing, CervixOmissieType type)
		{
			this.preferenceKeyWachttijdWaarschuwing = preferenceKeyWachttijdWaarschuwing;
			setLabformulier();
			omissieType = type;
			setPreferenceKeyWachttijd(preferenceKeyWachttijd, type);
		}

		protected Omissie(PreferenceKey preferenceKeyWachttijd, CervixOmissieType type)
		{
			this.preferenceKeyWachttijdWaarschuwing = null;
			setLabformulier();
			omissieType = type;
			setPreferenceKeyWachttijd(preferenceKeyWachttijd, type);
		}

		protected abstract boolean vanToepassing();

		protected abstract Date getStartWachttijd();

		protected abstract void voerActieUit();

		protected abstract void verstuurWaarschuwing();

		public BriefType getBriefType()
		{
			return briefType;
		}

		public HuisartsBerichtType getHuisartsBerichtType()
		{
			return huisartsBerichtType;
		}

		public CervixHuisartsBericht getUitstrijkjeOntbreektHuisartsBericht()
		{
			return uitstrijkjeOntbreektHuisartsBericht;
		}

		public CervixOmissieType getOmissieType()
		{
			return omissieType;
		}

		public void setPreferenceKeyWachttijd(PreferenceKey preferenceKeyWachttijd, CervixOmissieType type)
		{
			if (labformulier != null)
			{
				switch (type)
				{
				case UITSTRIJKJE_ONTBREEKT:
					this.preferenceKeyWachttijd = labformulier.getDigitaal() ? PreferenceKey.CERVIX_WACHTTIJD_UITSTRIJKJE_ONTBREEKT_DIGITAAL
						: PreferenceKey.CERVIX_WACHTTIJD_UITSTRIJKJE_ONTBREEKT_ANALOOG;
					return;
				case WACHT_OP_UITSTRIJKJE_ONTVANGEN:
					this.preferenceKeyWachttijd = labformulier.getDigitaal() ? PreferenceKey.CERVIX_WACHTTIJD_WACHT_OP_UITSTRIJKJE_ONTVANGEN_DIGITAAL
						: PreferenceKey.CERVIX_WACHTTIJD_WACHT_OP_UITSTRIJKJE_ONTVANGEN_ANALOOG;
					return;
				}
			}
			this.preferenceKeyWachttijd = preferenceKeyWachttijd;

		}

		public void setLabformulier()
		{
			if (monster instanceof CervixUitstrijkje)
			{
				labformulier = ((CervixUitstrijkje) monster).getLabformulier();
			}
		}

		public long bepaalWerkdagenTotOmissie()
		{
			var eindWachttijd = DateUtil.plusWerkdagen(getStartWachttijd(), preferenceService.getInteger(preferenceKeyWachttijd.name()));
			return DateUtil.getDaysBetweenIgnoreWeekends(dateSupplier.getDate(), eindWachttijd, true);
		}
	}

}
