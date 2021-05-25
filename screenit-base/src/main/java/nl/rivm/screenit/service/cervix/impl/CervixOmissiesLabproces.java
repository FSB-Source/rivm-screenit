package nl.rivm.screenit.service.cervix.impl;

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

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.annotation.PostConstruct;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.WerkDagHelper;
import nl.rivm.screenit.model.cervix.CervixHuisartsBericht;
import nl.rivm.screenit.model.cervix.CervixLabformulier;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.enums.CervixHpvUitslag;
import nl.rivm.screenit.model.cervix.enums.CervixLabformulierStatus;
import nl.rivm.screenit.model.cervix.enums.CervixOmissieType;
import nl.rivm.screenit.model.cervix.enums.CervixUitstrijkjeStatus;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.HuisartsBerichtType;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.MailService;
import nl.rivm.screenit.service.cervix.CervixHuisartsBerichtService;
import nl.rivm.screenit.service.cervix.CervixMailService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.cervix.CervixMonsterUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.joda.time.DateTime;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

@Component
@Scope("prototype")
public class CervixOmissiesLabproces
{
	private static final Logger LOG = LoggerFactory.getLogger(CervixOmissiesLabproces.class);

	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private CervixHuisartsBerichtService huisartsBerichtService;

	@Autowired
	private MailService mailService;

	@Autowired
	private CervixMailService cervixMailService;

	private CervixMonster monster;

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
		for (Omissie omissie : omissieLijst)
		{
			if (omissie.vanToepassing())
			{
				PreferenceKey preferenceKeyWachttijd = omissie.preferenceKeyWachttijd;

				DateTime eindWachttijd = WerkDagHelper.plusBusinessDays(new DateTime(omissie.getStartWachttijd()),
					preferenceService.getInteger(preferenceKeyWachttijd.name()));

				if (dateSupplier.getDateTime().isAfter(eindWachttijd))
				{
					omissie.voerActieUit();
					LOG.info("Omissie " + preferenceKeyWachttijd.name() + " uitgevoerd voor client met id "
						+ monster.getOntvangstScreeningRonde().getDossier().getClient().getId());
					return omissie;
				}

				PreferenceKey preferenceKeyWachttijdWaarschuwing = omissie.preferenceKeyWachttijdWaarschuwing;
				if (preferenceKeyWachttijdWaarschuwing != null)
				{
					DateTime eindWachttijdWaarschuwing = WerkDagHelper.plusBusinessDays(new DateTime(omissie.getStartWachttijd()),
						preferenceService.getInteger(preferenceKeyWachttijdWaarschuwing.name()));
					if (dateSupplier.getDateTime().isAfter(eindWachttijdWaarschuwing))
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
				CervixHuisartsBericht huisartsBericht = huisartsBerichtService.maakCervixHuisartsBericht(
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
				CervixMonster monsterHpvUitslag = monster.getOntvangstScreeningRonde().getMonsterHpvUitslag();
				if (monsterHpvUitslag != null && monsterHpvUitslag.getLaatsteHpvBeoordeling().getHpvUitslag() == CervixHpvUitslag.POSITIEF)
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
				CervixLabformulier kunstmatigLabformulier = new CervixLabformulier();
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
				CervixMonster monsterHpvUitslag = monster.getOntvangstScreeningRonde().getMonsterHpvUitslag();
				if (monsterHpvUitslag != null && monsterHpvUitslag.getLaatsteHpvBeoordeling().getHpvUitslag() == CervixHpvUitslag.POSITIEF)
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
				CervixMonster monsterHpvUitslag = monster.getOntvangstScreeningRonde().getMonsterHpvUitslag();
				if (monsterHpvUitslag != null && monsterHpvUitslag.getLaatsteHpvBeoordeling().getHpvUitslag() == CervixHpvUitslag.POSITIEF)
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
					CervixUitstrijkje uitstrijkje = CervixMonsterUtil.getUitstrijkje(monster);
					if (uitstrijkje.getOntvangstdatum() != null && uitstrijkje.getCytologieOrder() != null)
					{
						return true;
					}
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
				CervixUitstrijkje uitstrijkje = CervixMonsterUtil.getUitstrijkje(monster);
				SimpleDateFormat dateFormat = Constants.getDateTimeSecondsFormat();

				Integer wachtOp = preferenceService.getInteger(preferenceKeyWachttijd.name());
				Integer wachtOpWaarschuwing = preferenceService.getInteger(preferenceKeyWachttijdWaarschuwing.name());

				String content = "Geachte heer, mevrouw,<br /><br />" +
					"Volgens onze informatie hebben we van onderstaand monsterID binnen het bevolkingsonderzoek baarmoederhalskanker nog geen cytologieverslag ontvangen.<br /><br />"
					+
					"BVO Monster ID: {monsterID}<br />" +
					"Monster in lab ontvangen op: {monsterOntvangst}<br />" +
					"Cytologie-order verstuurd op: {orderVerstuurd}<br /><br />" +
					"De datum waarop het monster door {labNaam} is ontvangen is minimaal {wachtOpWaarschuwingCytoUitslag} werkdagen geleden.<br />" +
					"Deze (dagelijkse) mail blijft terugkomen totdat in ScreenIT het cytologieverslag is binnengekomen en verwerkt, of totdat er {wachtOpCytoUitslag} werkdagen zijn verstreken sinds de ontvangst van het monster. Indien dat laatste het geval is, ontvangen huisarts en cli&euml;nt een uitslag (cytologisch) onbeoordeelbaar.<br /><br />"
					+
					"Voor vragen met betrekking tot dit bericht kunt u zich wenden tot de helpdesk van LBO via het e-mail adres helpdesk@fsb-lbo.nl";

				content = content.replaceAll("\\{monsterID\\}", monster.getMonsterId());
				content = content.replaceAll("\\{monsterOntvangst\\}", dateFormat.format(monster.getOntvangstdatum()));
				String orderVerstuurd = dateFormat.format(uitstrijkje.getCytologieOrder().getStatusDatum());
				content = content.replaceAll("\\{orderVerstuurd\\}", orderVerstuurd);
				content = content.replaceAll("\\{wachtOpCytoUitslag\\}", wachtOp.toString());
				content = content.replaceAll("\\{wachtOpWaarschuwingCytoUitslag\\}", wachtOpWaarschuwing.toString());
				content = content.replaceAll("\\{labNaam\\}", monster.getLaboratorium().getNaam());

				String subject = "Ontbrekend cytologieverslag";

				mailService.sendEmail(uitstrijkje.getLaboratorium().getBmhkLabWarnMail(), subject, content);
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

		public Omissie(PreferenceKey preferenceKeyWachttijd, PreferenceKey preferenceKeyWachttijdWaarschuwing, CervixOmissieType type)
		{
			this.preferenceKeyWachttijdWaarschuwing = preferenceKeyWachttijdWaarschuwing;
			setLabformulier();
			omissieType = type;
			setPreferenceKeyWachttijd(preferenceKeyWachttijd, type);
		}

		public Omissie(PreferenceKey preferenceKeyWachttijd, CervixOmissieType type)
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
			DateTime eindWachttijd = WerkDagHelper.plusBusinessDays(new DateTime(getStartWachttijd()),
				preferenceService.getInteger(preferenceKeyWachttijd.name()));
			return WerkDagHelper.werkdagenTotDatum(dateSupplier.getLocalDate(), DateUtil.toLocalDate(eindWachttijd.toDate()));
		}
	}

}
