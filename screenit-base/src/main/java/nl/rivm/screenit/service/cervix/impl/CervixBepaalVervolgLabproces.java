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

import nl.rivm.screenit.model.cervix.enums.CervixHpvBeoordelingWaarde;
import nl.rivm.screenit.service.cervix.enums.CervixVervolgTekst;

class CervixBepaalVervolgLabproces
{

	private CervixBepaalVervolgContext context;

	private CervixVervolg vervolg = new CervixVervolg();

	private CervixBepaalGevolgenLabproces bepaalGevolgenLabproces;

	public CervixBepaalVervolgLabproces(CervixBepaalVervolgContext context)
	{
		this.context = context;
		bepaalGevolgenLabproces = new CervixBepaalGevolgenLabproces(context, vervolg);
	}

	public CervixVervolg bepaalVervolg()
	{
		try
		{
			switch (context.huidigMonsterType)
			{
			case UITSTRIJKJE:
				if (context.huidigeMonsterBrief == null)
				{
					bepaalVervolgUitstrijkjeVervolgonderzoek();
				}
				else
				{
					if (context.monsterHpvUitslag != null && context.huidigeMonster.equals(context.monsterHpvUitslag)
						|| context.uitstrijkjeCytologieUitslag != null && context.huidigeMonster.equals(context.uitstrijkjeCytologieUitslag)
						|| context.uitstrijkjeVervolgonderzoekUitslag != null && context.huidigeMonster.equals(context.uitstrijkjeVervolgonderzoekUitslag))
					{
						vervolg.setVervolgTekst(CervixVervolgTekst.UITSTRIJKJE_CLIENT_REEDS_GEINFORMEERD_BEWAAR);
					}
					else
					{
						vervolg.setVervolgTekst(CervixVervolgTekst.UITSTRIJKJE_CLIENT_REEDS_GEINFORMEERD_VERNIETIG);
					}
				}
				break;
			case ZAS:
				if (context.huidigeMonsterBrief == null)
				{
					bepaalVervolgZasVervolgonderzoek();
				}
				else
				{
					if (context.monsterHpvUitslag != null && context.huidigeMonster.equals(context.monsterHpvUitslag))
					{
						if (CervixHpvBeoordelingWaarde.POSITIEF.equals(context.hpvUitslag))
						{
							vervolg.setVervolgTekst(CervixVervolgTekst.ZAS_HPV_POSITIEF_CLIENT_REEDS_GEINFORMEERD_BEWAAR);
						}
						else
						{
							vervolg.setVervolgTekst(CervixVervolgTekst.ZAS_HPV_NEGATIEF_CLIENT_REEDS_GEINFORMEERD_BEWAAR);
						}

					}
					else
					{
						vervolg.setVervolgTekst(CervixVervolgTekst.ZAS_CLIENT_REEDS_GEINFORMEERD_VERNIETIG);
					}
				}
				break;
			default:
				throw new IllegalStateException();
			}

			if (vervolg == null)
			{
				throw new IllegalStateException();
			}
		}
		catch (IllegalStateException e)
		{
			throw e;
		}
		catch (Exception e)
		{
			throw new IllegalStateException(e);
		}
		return vervolg;
	}

	private void bepaalVervolgUitstrijkjeVervolgonderzoek()
	{
		if (!context.huidigeOntvangstInVervolgonderzoek)
		{
			bepaalVervolgUitstrijkjeCytologie();
		}
		else
		{
			if (context.vervolgonderzoekUitslag != null)
			{
				if (context.huidigeMonster.equals(context.uitstrijkjeVervolgonderzoekUitslag))
				{
					vervolg.setVervolgTekst(CervixVervolgTekst.UITSTRIJKJE_CYTOLOGIE_UITSLAG_BEWAAR);
					bepaalGevolgenLabproces.bepaalGevolgenUitstijkjeVervolgonderzoekUitslag();
				}
				else
				{
					vervolg.setVervolgTekst(CervixVervolgTekst.UITSTRIJKJE_REEDS_CYTOLOGIE_UITSLAG_VERNIETIG);
					bepaalGevolgenLabproces.bepaalGevolgenUitstijkjeReedsVervolgonderzoekUitslag();
				}
			}
			else
			{
				switch (context.huidigUitstrijkjeStatus)
				{
				case NIET_ONTVANGEN:
					vervolg.setVervolgTekst(CervixVervolgTekst.UITSTRIJKJE_REGISTREER_ONTVANGST);
					break;
				case ONTVANGEN:
					vervolg.setVervolgTekst(CervixVervolgTekst.UITSTRIJKJE_VERVOLGONDERZOEK_NAAR_CYTOLOGIE);
					break;
				case BEOORDEELD_DOOR_CYTOLOGIE:
					switch (context.huidigUitstrijkje.getCytologieVerslag().getCytologieUitslag())
					{
					case PAP0:
						vervolg.setVervolgTekst(CervixVervolgTekst.UITSTRIJKJE_CYTOLOGIE_ONBEOORDEELBAAR_VERNIETIG);
						bepaalGevolgenLabproces.bepaalGevolgenUitstijkjeVervolgonderzoekOnbeoordeelbaar();
						break;
					default:
						throw new IllegalStateException();
					}
					break;
				case NIET_ANALYSEERBAAR:
					vervolg.setVervolgTekst(CervixVervolgTekst.UITSTRIJKJE_NIET_ANALYSEERBAAR);
					bepaalGevolgenLabproces.bepaalGevolgenUitstijkjeInVervolgonderzoekNietAnalyseerbaar();
					break;
				default:
					throw new IllegalStateException();
				}
			}
		}
	}

	private void bepaalVervolgUitstrijkjeCytologie()
	{
		if (!CervixHpvBeoordelingWaarde.POSITIEF.equals(context.hpvUitslag) || context.hpvUitslagVanZasNogBriefVerstuurd)
		{
			bepaalVervolgUitstrijkjeHpv();
		}
		else
		{
			if (context.cytologieUitslag != null)
			{
				if (context.huidigeMonster.equals(context.uitstrijkjeCytologieUitslag))
				{
					vervolg.setVervolgTekst(CervixVervolgTekst.UITSTRIJKJE_CYTOLOGIE_UITSLAG_BEWAAR);
					bepaalGevolgenLabproces.bepaalGevolgenUitstijkjeCytologieUitslag();
				}
				else
				{
					vervolg.setVervolgTekst(CervixVervolgTekst.UITSTRIJKJE_REEDS_CYTOLOGIE_UITSLAG_VERNIETIG);
					bepaalGevolgenLabproces.bepaalGevolgenUitstijkjeReedsCytologieUitslag();
				}
			}
			else
			{
				switch (context.huidigUitstrijkjeStatus)
				{
				case NIET_ONTVANGEN:
					vervolg.setVervolgTekst(CervixVervolgTekst.UITSTRIJKJE_REGISTREER_ONTVANGST);
					break;
				case ONTVANGEN:
				case GEANALYSEERD_OP_HPV_POGING_1:
				case GEANALYSEERD_OP_HPV_POGING_2:
					if (context.huidigeMonster.equals(context.monsterHpvUitslag))
					{
						vervolg.setVervolgTekst(CervixVervolgTekst.UITSTRIJKJE_HPV_POSITIEF_NAAR_CYTOLOGIE);
					}
					else
					{
						vervolg.setVervolgTekst(CervixVervolgTekst.UITSTRIJKJE_REEDS_HPV_UITSLAG_NAAR_CYTOLOGIE);
					}
					break;
				case BEOORDEELD_DOOR_CYTOLOGIE:
					switch (context.huidigUitstrijkje.getCytologieVerslag().getCytologieUitslag())
					{
					case PAP0:
						if (context.huidigeMonster.equals(context.monsterHpvUitslag))
						{
							vervolg.setVervolgTekst(CervixVervolgTekst.UITSTRIJKJE_HPV_POSITIEF_CYTOLOGIE_ONBEOORDEELBAAR_BEWAAR);
						}
						else
						{
							vervolg.setVervolgTekst(CervixVervolgTekst.UITSTRIJKJE_CYTOLOGIE_ONBEOORDEELBAAR_VERNIETIG);
						}
						bepaalGevolgenLabproces.bepaalGevolgenUitstrijkjeCytologieOnbeoordeelbaar();
						break;
					default:
						throw new IllegalStateException();
					}
					break;
				case NIET_ANALYSEERBAAR:
					vervolg.setVervolgTekst(CervixVervolgTekst.UITSTRIJKJE_NIET_ANALYSEERBAAR);
					bepaalGevolgenLabproces.bepaalGevolgenUitstrijkjeHpvPositiefNietAnalyseerbaar();
					break;
				default:
					throw new IllegalStateException();
				}
			}
		}
	}

	private void bepaalVervolgUitstrijkjeHpv()
	{
		if (context.hpvUitslag != null && !context.hpvUitslagVanZasNogBriefVerstuurd)
		{

			if (context.huidigeMonster.equals(context.monsterHpvUitslag))
			{
				vervolg.setVervolgTekst(CervixVervolgTekst.UITSTRIJKJE_HPV_NEGATIEF_BEWAAR);
				bepaalGevolgenLabproces.bepaalGevolgenUitstrijkjeHpvUitslagNegatief();
			}
			else
			{
				vervolg.setVervolgTekst(CervixVervolgTekst.UITSTRIJKJE_REEDS_HPV_UITSLAG_VERNIETIG);
				bepaalGevolgenLabproces.bepaalGevolgenUitstrijkjeReedsHpvUitslagNegatief();
			}
		}
		else
		{
			switch (context.huidigUitstrijkjeStatus)
			{
			case NIET_ANALYSEERBAAR:
				vervolg.setVervolgTekst(CervixVervolgTekst.UITSTRIJKJE_NIET_ANALYSEERBAAR);
				bepaalGevolgenLabproces.bepaalGevolgenUitstrijkjeNietAnalyseerbaar();
				break;
			case NIET_ONTVANGEN:
				vervolg.setVervolgTekst(CervixVervolgTekst.UITSTRIJKJE_REGISTREER_ONTVANGST);
				break;
			case ONTVANGEN:
				vervolg.setVervolgTekst(CervixVervolgTekst.UITSTRIJKJE_ONTVANGEN_NAAR_HPV);
				break;
			case GEANALYSEERD_OP_HPV_POGING_1:
				switch (context.huidigUitstrijkje.getLaatsteHpvBeoordeling().getHpvUitslag())
				{
				case ONGELDIG:
					vervolg.setVervolgTekst(CervixVervolgTekst.UITSTRIJKJE_GEANALYSEERD_OP_HPV_POGING_1_ONGELDIG_NAAR_HPV);
					break;
				default:
					throw new IllegalStateException();
				}
				break;
			case GEANALYSEERD_OP_HPV_POGING_2:
				switch (context.huidigUitstrijkje.getLaatsteHpvBeoordeling().getHpvUitslag())
				{
				case ONGELDIG:
					vervolg.setVervolgTekst(CervixVervolgTekst.UITSTRIJKJE_GEANALYSEERD_OP_HPV_POGING_2_ONGELDIG_VERNIETIG);
					bepaalGevolgenLabproces.bepaalGevolgenUitstrijkjeHpvOnbeoordeelbaar();
					break;
				default:
					throw new IllegalStateException();
				}
				break;
			default:
				throw new IllegalStateException();
			}
		}
	}

	private void bepaalVervolgZasVervolgonderzoek()
	{
		if (!context.huidigeOntvangstInVervolgonderzoek)
		{
			bepaalVervolgZasCytologie();
		}
		else
		{
			vervolg.setVervolgTekst(CervixVervolgTekst.ZAS_REEDS_HPV_UITSLAG_VERNIETIG);
			bepaalGevolgenLabproces.bepaalGevolgenZasReedsVervolgonderzoek();
		}
	}

	private void bepaalVervolgZasCytologie()
	{
		if (!CervixHpvBeoordelingWaarde.POSITIEF.equals(context.hpvUitslag))
		{
			bepaalVervolgZasHpv();
		}
		else
		{
			if (context.huidigeMonster.equals(context.monsterHpvUitslag))
			{
				vervolg.setVervolgTekst(CervixVervolgTekst.ZAS_HPV_POSITIEF_BEWAAR);
				bepaalGevolgenLabproces.bepaalGevolgenZasReedsHpvUitslagPositief();
			}
			else
			{
				vervolg.setVervolgTekst(CervixVervolgTekst.ZAS_REEDS_HPV_UITSLAG_VERNIETIG);
				bepaalGevolgenLabproces.bepaalGevolgenZasReedsHpvUitslagPositief();
			}

		}
	}

	private void bepaalVervolgZasHpv()
	{
		if (context.hpvUitslag != null)
		{

			if (context.huidigeMonster.equals(context.monsterHpvUitslag))
			{
				vervolg.setVervolgTekst(CervixVervolgTekst.ZAS_HPV_NEGATIEF_BEWAAR);
				bepaalGevolgenLabproces.bepaalGevolgenZasHpvNegatief();
			}
			else
			{
				vervolg.setVervolgTekst(CervixVervolgTekst.ZAS_REEDS_HPV_UITSLAG_VERNIETIG);
				bepaalGevolgenLabproces.bepaalGevolgenZasReedsHpvUitslagNegatief();
			}
		}
		else if (context.huidigeZasStatus != null)
		{
			switch (context.huidigeZasStatus)
			{
			case NIET_ANALYSEERBAAR:
				if (context.isZasHoudbaar)
				{
					vervolg.setVervolgTekst(CervixVervolgTekst.ZAS_NIET_ANALYSEERBAAR_VERNIETIG);
				}
				else
				{
					vervolg.setVervolgTekst(CervixVervolgTekst.ZAS_NIET_HOUDBAAR_VERNIETIG);
				}
				bepaalGevolgenLabproces.bepaalGevolgenZasNietAnalyseerbaar();
				break;
			case VERSTUURD:
				vervolg.setVervolgTekst(CervixVervolgTekst.ZAS_REGISTREER_ONTVANGST);
				break;
			case ONTVANGEN:
				vervolg.setVervolgTekst(CervixVervolgTekst.ZAS_ONTVANGEN_NAAR_HPV);
				break;
			case GEANALYSEERD_OP_HPV_POGING_1:
				switch (context.huidigeZas.getLaatsteHpvBeoordeling().getHpvUitslag())
				{
				case ONGELDIG:
					vervolg.setVervolgTekst(CervixVervolgTekst.ZAS_GEANALYSEERD_OP_HPV_POGING_1_ONGELDIG_NAAR_HPV);
					break;
				default:
					throw new IllegalStateException();
				}
				break;
			case GEANALYSEERD_OP_HPV_POGING_2:
				switch (context.huidigeZas.getLaatsteHpvBeoordeling().getHpvUitslag())
				{
				case ONGELDIG:
					vervolg.setVervolgTekst(CervixVervolgTekst.ZAS_GEANALYSEERD_OP_HPV_POGING_2_ONGELDIG_VERNIETIG);
					bepaalGevolgenLabproces.bepaalGevolgenZasHpvOnbeoordeelbaar();
					break;
				default:
					throw new IllegalStateException();
				}
				break;
			default:
				throw new IllegalStateException();
			}
		}
	}
}
