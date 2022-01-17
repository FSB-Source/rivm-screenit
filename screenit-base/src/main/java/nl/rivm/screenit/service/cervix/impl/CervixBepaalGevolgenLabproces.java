package nl.rivm.screenit.service.cervix.impl;

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

import nl.rivm.screenit.dao.cervix.CervixBepaalVervolgDao;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.HuisartsBerichtType;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.spring.injection.SpringBeanProvider;

class CervixBepaalGevolgenLabproces
{

	CervixBepaalVervolgContext context;

	CervixVervolg vervolg;

	CervixBepaalGevolgenLabproces(CervixBepaalVervolgContext context, CervixVervolg vervolg)
	{
		this.context = context;
		this.vervolg = vervolg;
	}

	void bepaalGevolgenUitstijkjeVervolgonderzoekUitslag()
	{
		switch (context.vervolgonderzoekUitslag)
		{
		case PAP1:
			vervolg.setVervolg(BriefType.CERVIX_CONTROLEUITSTRIJKJE_NEGATIEF, HuisartsBerichtType.CERVIX_VERVOLGONDERZOEK_CYTOLOGIE_UITSLAG, true); 
			break;
		case PAP2:
		case PAP3A1:
			vervolg.setVervolg(BriefType.CERVIX_CONTROLEUITSTRIJKJE_LICHTE_AFWIJKING, HuisartsBerichtType.CERVIX_VERVOLGONDERZOEK_CYTOLOGIE_UITSLAG, true); 
			break;
		case PAP3A2:
		case PAP3B:
		case PAP4:
		case PAP5:
			vervolg.setVervolg(BriefType.CERVIX_CONTROLEUITSTRIJKJE_AFWIJKING, HuisartsBerichtType.CERVIX_VERVOLGONDERZOEK_CYTOLOGIE_UITSLAG, true); 
			break;
		default:
			throw new IllegalStateException();
		}
	}

	void bepaalGevolgenUitstijkjeReedsVervolgonderzoekUitslag()
	{
		switch (context.vervolgonderzoekUitslag)
		{
		case PAP1:
			vervolg.setVervolg(BriefType.CERVIX_MONSTER_NA_HPV_NEGATIEF, HuisartsBerichtType.CERVIX_UITSLAG_REEDS_BEKEND); 
			break;
		case PAP2:
		case PAP3A1:
		case PAP3A2:
		case PAP3B:
		case PAP4:
		case PAP5:
			vervolg.setVervolg(BriefType.CERVIX_ZAS_NA_CYTOLOGIE_POSITIEF, HuisartsBerichtType.CERVIX_UITSLAG_REEDS_BEKEND); 
			break;
		default:
			throw new IllegalStateException();
		}
	}

	void bepaalGevolgenUitstijkjeVervolgonderzoekOnbeoordeelbaar()
	{
		CervixBepaalVervolgDao bepaalVervolgDao = SpringBeanProvider.getInstance().getBean(CervixBepaalVervolgDao.class);
		if (!bepaalVervolgDao.anderUitstrijkjeOnbeoordeelbaar(context.huidigUitstrijkje))
		{
			vervolg.setVervolg(BriefType.CERVIX_UITSTRIJKJE_NIET_ANALYSEERBAAR_OF_CYTOLOGIE_ONBEOORDEELBAAR, HuisartsBerichtType.CERVIX_VERVOLGONDERZOEK_CYTOLOGIE_UITSLAG); 
		}
		else
		{
			vervolg.setVervolg(BriefType.CERVIX_UITSTRIJKJE_TWEEDE_KEER_ONBEOORDEELBAAR, HuisartsBerichtType.CERVIX_TWEEDE_UITSTRIJKJE_CYTOLOGIE_ONBEOORDEELBAAR); 
		}
	}

	void bepaalGevolgenUitstijkjeCytologieUitslag()
	{
		switch (context.cytologieUitslag)
		{
		case PAP1:
			ICurrentDateSupplier dateSupplier = SpringBeanProvider.getInstance().getBean(ICurrentDateSupplier.class);
			if (context.huidigeMonster.equals(context.monsterHpvUitslag))
			{
				vervolg.setVervolg(BriefType.CERVIX_CYTOLOGIE_NEGATIEF, HuisartsBerichtType.CERVIX_UITSTRIJKJE_CYTOLOGIE_UITSLAG, dateSupplier.getDate()); 
			}
			else
			{
				vervolg.setVervolg(BriefType.CERVIX_VOLGEND_MONSTER_CYTOLOGIE_NEGATIEF, HuisartsBerichtType.CERVIX_UITSTRIJKJE_CYTOLOGIE_UITSLAG, dateSupplier.getDate()); 
			}
			break;
		case PAP2:
		case PAP3A1:
			if (context.huidigeMonster.equals(context.monsterHpvUitslag))
			{
				vervolg.setVervolg(BriefType.CERVIX_CYTOLOGIE_LICHTE_AFWIJKING, HuisartsBerichtType.CERVIX_UITSTRIJKJE_CYTOLOGIE_UITSLAG, true); 
			}
			else
			{
				vervolg.setVervolg(BriefType.CERVIX_VOLGEND_MONSTER_CYTOLOGIE_LICHTE_AFWIJKING, HuisartsBerichtType.CERVIX_UITSTRIJKJE_CYTOLOGIE_UITSLAG, true); 
			}
			break;
		case PAP3A2:
		case PAP3B:
		case PAP4:
		case PAP5:
			if (context.huidigeMonster.equals(context.monsterHpvUitslag))
			{
				vervolg.setVervolg(BriefType.CERVIX_CYTOLOGIE_AFWIJKING, HuisartsBerichtType.CERVIX_UITSTRIJKJE_CYTOLOGIE_UITSLAG, true); 
			}
			else
			{
				vervolg.setVervolg(BriefType.CERVIX_VOLGEND_MONSTER_CYTOLOGIE_AFWIJKING, HuisartsBerichtType.CERVIX_UITSTRIJKJE_CYTOLOGIE_UITSLAG, true); 
			}
			break;
		default:
			throw new IllegalStateException();
		}
	}

	void bepaalGevolgenUitstijkjeReedsCytologieUitslag()
	{
		vervolg.setVervolg(BriefType.CERVIX_ZAS_NA_CYTOLOGIE_POSITIEF, HuisartsBerichtType.CERVIX_UITSLAG_REEDS_BEKEND); 
	}

	void bepaalGevolgenUitstrijkjeCytologieOnbeoordeelbaar()
	{
		if (context.huidigeMonster.equals(context.monsterHpvUitslag))
		{
			vervolg.setVervolg(BriefType.CERVIX_CYTOLOGIE_ONBEOORDEELBAAR, HuisartsBerichtType.CERVIX_UITSTRIJKJE_CYTOLOGIE_UITSLAG); 
		}
		else
		{
			CervixBepaalVervolgDao bepaalVervolgDao = SpringBeanProvider.getInstance().getBean(CervixBepaalVervolgDao.class);
			if (!bepaalVervolgDao.anderUitstrijkjeOnbeoordeelbaar(context.huidigUitstrijkje))
			{
				vervolg.setVervolg(BriefType.CERVIX_UITSTRIJKJE_NIET_ANALYSEERBAAR_OF_CYTOLOGIE_ONBEOORDEELBAAR, HuisartsBerichtType.CERVIX_UITSTRIJKJE_CYTOLOGIE_UITSLAG); 
			}
			else
			{
				vervolg.setVervolg(BriefType.CERVIX_UITSTRIJKJE_TWEEDE_KEER_ONBEOORDEELBAAR, HuisartsBerichtType.CERVIX_TWEEDE_UITSTRIJKJE_CYTOLOGIE_ONBEOORDEELBAAR); 
			}
		}
	}

	void bepaalGevolgenUitstrijkjeHpvUitslagNegatief()
	{
		switch (context.ontvangstRonde.getLeeftijdcategorie())
		{
		case _30:
		case _35:
		case _45:
		case _55:
			vervolg.setVervolg(BriefType.CERVIX_HPV_NEGATIEF_VOLGENDE_RONDE_5_JAAR, HuisartsBerichtType.CERVIX_UITSTRIJKJE_HPV_NEGATIEF, true); 
			break;
		case _40:
		case _50:
			vervolg.setVervolg(BriefType.CERVIX_HPV_NEGATIEF_VOLGENDE_RONDE_10_JAAR, HuisartsBerichtType.CERVIX_UITSTRIJKJE_HPV_NEGATIEF, true,
				context.ontvangstRonde.getLeeftijdcategorie().volgende().volgende()); 
			break;
		case _60:
		case _65:
			vervolg.setVervolg(BriefType.CERVIX_HPV_NEGATIEF_LAATSTE_RONDE, HuisartsBerichtType.CERVIX_UITSTRIJKJE_HPV_NEGATIEF, true); 
			break;
		default:
			throw new IllegalStateException();
		}
	}

	void bepaalGevolgenUitstrijkjeReedsHpvUitslagNegatief()
	{
		vervolg.setVervolg(BriefType.CERVIX_MONSTER_NA_HPV_NEGATIEF, HuisartsBerichtType.CERVIX_UITSLAG_REEDS_BEKEND); 
	}

	void bepaalGevolgenUitstrijkjeHpvOnbeoordeelbaar()
	{
		CervixBepaalVervolgDao bepaalVervolgDao = SpringBeanProvider.getInstance().getBean(CervixBepaalVervolgDao.class);
		if (!bepaalVervolgDao.anderUitstrijkjeOnbeoordeelbaar(context.huidigUitstrijkje))
		{
			vervolg.setVervolg(BriefType.CERVIX_UITSTRIJKJE_NIET_ANALYSEERBAAR_OF_HPV_ONBEOORDEELBAAR, HuisartsBerichtType.CERVIX_UITSTRIJKJE_HPV_ONBEOORDEELBAAR); 
		}
		else
		{
			vervolg.setVervolg(BriefType.CERVIX_UITSTRIJKJE_TWEEDE_KEER_ONBEOORDEELBAAR, HuisartsBerichtType.CERVIX_TWEEDE_UITSTRIJKJE_HPV_ONBEOORDEELBAAR); 
		}
	}

	void bepaalGevolgenUitstijkjeInVervolgonderzoekNietAnalyseerbaar()
	{
		vervolg.setVervolg(BriefType.CERVIX_UITSTRIJKJE_NIET_ANALYSEERBAAR_OF_CYTOLOGIE_ONBEOORDEELBAAR, HuisartsBerichtType.CERVIX_UITSTRIJKJE_NIET_ANALYSEERBAAR); 
	}

	void bepaalGevolgenUitstrijkjeHpvPositiefNietAnalyseerbaar()
	{
		vervolg.setVervolg(BriefType.CERVIX_UITSTRIJKJE_NIET_ANALYSEERBAAR_OF_CYTOLOGIE_ONBEOORDEELBAAR, HuisartsBerichtType.CERVIX_UITSTRIJKJE_NIET_ANALYSEERBAAR); 
	}

	void bepaalGevolgenUitstrijkjeNietAnalyseerbaar()
	{
		vervolg.setVervolg(BriefType.CERVIX_UITSTRIJKJE_NIET_ANALYSEERBAAR_OF_HPV_ONBEOORDEELBAAR, HuisartsBerichtType.CERVIX_UITSTRIJKJE_NIET_ANALYSEERBAAR); 
	}

	void bepaalGevolgenZasReedsVervolgonderzoek()
	{
		if (context.vervolgonderzoekUitslag != null)
		{
			switch (context.vervolgonderzoekUitslag)
			{
			case PAP1:
				vervolg.setVervolg(BriefType.CERVIX_MONSTER_NA_HPV_NEGATIEF); 
				break;
			case PAP2:
			case PAP3A1:
			case PAP3A2:
			case PAP3B:
			case PAP4:
			case PAP5:
				vervolg.setVervolg(BriefType.CERVIX_ZAS_NA_CYTOLOGIE_POSITIEF); 
				break;
			case PAP0:
			default:
				throw new IllegalStateException();
			}
		}
		else
		{
			CervixBepaalVervolgDao bepaalVervolgDao = SpringBeanProvider.getInstance().getBean(CervixBepaalVervolgDao.class);
			if (bepaalVervolgDao.uitstrijkjeOnbeoordeelbaarCytologie(context.ontvangstRonde))
			{
				vervolg.setVervolg(BriefType.CERVIX_ZAS_NA_CYTOLOGIE_ONBEOORDEELBAAR); 
			}
			else
			{
				bepaalGevolgenZasReedsCytologie();
			}
		}
	}

	void bepaalGevolgenZasReedsHpvUitslagPositief()
	{
		if (context.cytologieUitslag != null)
		{
			bepaalGevolgenZasReedsCytologie();
		}
		else
		{
			CervixBepaalVervolgDao bepaalVervolgDao = SpringBeanProvider.getInstance().getBean(CervixBepaalVervolgDao.class);
			if (bepaalVervolgDao.uitstrijkjeOnbeoordeelbaarCytologie(context.ontvangstRonde))
			{
				vervolg.setVervolg(BriefType.CERVIX_ZAS_NA_CYTOLOGIE_ONBEOORDEELBAAR); 
			}
			else
			{
				if (context.huidigeMonster.equals(context.monsterHpvUitslag))
				{
					vervolg.setVervolg(BriefType.CERVIX_ZAS_HPV_POSITIEF); 
				}
				else
				{
					vervolg.setVervolg(BriefType.CERVIX_ZAS_NA_HPV_POSITIEF); 
				}
			}
		}
	}

	private void bepaalGevolgenZasReedsCytologie()
	{
		switch (context.cytologieUitslag)
		{
		case PAP1:
			vervolg.setVervolg(BriefType.CERVIX_ZAS_NA_CYTOLOGIE_NEGATIEF); 
			break;
		case PAP2:
		case PAP3A1:
		case PAP3A2:
		case PAP3B:
		case PAP4:
		case PAP5:
			vervolg.setVervolg(BriefType.CERVIX_ZAS_NA_CYTOLOGIE_POSITIEF); 
			break;
		case PAP0:
		default:
			throw new IllegalStateException();
		}
	}

	void bepaalGevolgenZasReedsHpvUitslagNegatief()
	{
		vervolg.setVervolg(BriefType.CERVIX_MONSTER_NA_HPV_NEGATIEF, HuisartsBerichtType.CERVIX_UITSLAG_REEDS_BEKEND); 
	}

	void bepaalGevolgenZasHpvNegatief()
	{
		switch (context.ontvangstRonde.getLeeftijdcategorie())
		{
		case _30:
		case _35:
		case _45:
		case _55:
			vervolg.setVervolg(BriefType.CERVIX_HPV_NEGATIEF_VOLGENDE_RONDE_5_JAAR, true); 
			break;
		case _40:
		case _50:
			vervolg.setVervolg(BriefType.CERVIX_HPV_NEGATIEF_VOLGENDE_RONDE_10_JAAR, true, context.ontvangstRonde.getLeeftijdcategorie().volgende().volgende()); 
			break;
		case _60:
		case _65:
			vervolg.setVervolg(BriefType.CERVIX_HPV_NEGATIEF_LAATSTE_RONDE, HuisartsBerichtType.CERVIX_UITSTRIJKJE_HPV_NEGATIEF, true);
			break;
		default:
			throw new IllegalStateException();
		}
	}

	void bepaalGevolgenZasHpvOnbeoordeelbaar()
	{
		CervixBepaalVervolgDao bepaalVervolgDao = SpringBeanProvider.getInstance().getBean(CervixBepaalVervolgDao.class);
		if (!bepaalVervolgDao.andereZasOngeldig(context.huidigeZas))
		{
			vervolg.setVervolg(BriefType.CERVIX_ZAS_NIET_ANALYSEERBAAR_OF_ONBEOORDEELBAAR); 
		}
		else
		{
			vervolg.setVervolg(BriefType.CERVIX_ZAS_TWEEDE_KEER_ONBEOORDEELBAAR); 
		}
	}

	void bepaalGevolgenZasNietAnalyseerbaar()
	{
		vervolg.setVervolg(BriefType.CERVIX_ZAS_NIET_ANALYSEERBAAR_OF_ONBEOORDEELBAAR); 
	}
}
