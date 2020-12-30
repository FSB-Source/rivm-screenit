package nl.rivm.screenit.service.cervix.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Date;

import nl.rivm.screenit.model.cervix.enums.CervixLeeftijdcategorie;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.HuisartsBerichtType;
import nl.rivm.screenit.service.cervix.enums.CervixVervolgTekst;

public class CervixVervolg
{

	private CervixVervolgTekst vervolgTekst = null;

	private BriefType briefType = null;

	private HuisartsBerichtType huisartsBerichtType = null;

	private Date inVervolgonderzoekDatum = null;

	private boolean sluitRonde = false;

	private CervixLeeftijdcategorie leeftijdcategorieVolgendeRonde = null;

	public void setVervolgTekst(CervixVervolgTekst vervolgTekst)
	{
		this.vervolgTekst = vervolgTekst;
	}

	public void setVervolg(BriefType briefType)
	{
		this.briefType = briefType;
	}

	public void setVervolg(BriefType briefType, HuisartsBerichtType huisartsBerichtType)
	{
		this.briefType = briefType;
		this.huisartsBerichtType = huisartsBerichtType;
	}

	public void setVervolg(BriefType briefType, boolean sluitRonde)
	{
		this.briefType = briefType;
		this.sluitRonde = sluitRonde;
	}

	public void setVervolg(BriefType briefType, boolean sluitRonde, CervixLeeftijdcategorie volgendeLeeftijdcategorie)
	{
		this.briefType = briefType;
		this.sluitRonde = sluitRonde;
		this.leeftijdcategorieVolgendeRonde = volgendeLeeftijdcategorie;
	}

	public void setVervolg(BriefType briefType, HuisartsBerichtType huisartsBerichtType, boolean sluitRonde)
	{
		this.briefType = briefType;
		this.huisartsBerichtType = huisartsBerichtType;
		this.sluitRonde = sluitRonde;
	}

	public void setVervolg(BriefType briefType, HuisartsBerichtType huisartsBerichtType, boolean sluitRonde,
		CervixLeeftijdcategorie volgendeLeeftijdcategorie)
	{
		this.briefType = briefType;
		this.huisartsBerichtType = huisartsBerichtType;
		this.sluitRonde = sluitRonde;
		this.leeftijdcategorieVolgendeRonde = volgendeLeeftijdcategorie;
	}

	public void setVervolg(BriefType briefType, HuisartsBerichtType huisartsBerichtType, Date inVervolgonderzoekDatum)
	{
		this.briefType = briefType;
		this.huisartsBerichtType = huisartsBerichtType;
		this.inVervolgonderzoekDatum = inVervolgonderzoekDatum;
	}

	public CervixVervolgTekst getVervolgTekst()
	{
		return vervolgTekst;
	}

	public BriefType getBriefType()
	{
		return briefType;
	}

	public HuisartsBerichtType getHuisartsBerichtType()
	{
		return huisartsBerichtType;
	}

	public Date getInVervolgonderzoekDatum()
	{
		return inVervolgonderzoekDatum;
	}

	public boolean sluitRonde()
	{
		return sluitRonde;
	}

	public CervixLeeftijdcategorie getLeeftijdcategorieVolgendeRonde()
	{
		return leeftijdcategorieVolgendeRonde;
	}
}
