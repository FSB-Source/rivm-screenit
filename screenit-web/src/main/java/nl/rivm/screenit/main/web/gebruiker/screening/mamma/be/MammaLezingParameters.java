package nl.rivm.screenit.main.web.gebruiker.screening.mamma.be;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import nl.rivm.screenit.model.mamma.enums.MammaAmputatie;

public class MammaLezingParameters
{
	private boolean inzien;

	private boolean verwijzenRechtsVerplicht;

	private boolean verwijzenLinksVerplicht;

	private boolean verbergAfrondKnop;

	private boolean toonOvernemenKnop;

	private boolean toonBiradsOpmerkingVeld;

	private boolean metAfbeelding;

	private MammaAmputatie amputatie;

	public static MammaLezingParameters maakAlleenInzien()
	{
		MammaLezingParameters lezingParameters = new MammaLezingParameters();
		lezingParameters.setInzien(true);
		lezingParameters.setToonBiradsOpmerkingVeld(true);
		return lezingParameters;
	}

	public boolean isInzien()
	{
		return inzien;
	}

	public MammaLezingParameters setInzien(boolean inzien)
	{
		this.inzien = inzien;
		return this; 
	}

	public boolean isVerwijzenRechtsVerplicht()
	{
		return verwijzenRechtsVerplicht;
	}

	public MammaLezingParameters setVerwijzenRechtsVerplicht(boolean verwijzenRechtsVerplicht)
	{
		this.verwijzenRechtsVerplicht = verwijzenRechtsVerplicht;
		return this; 
	}

	public boolean isVerwijzenLinksVerplicht()
	{
		return verwijzenLinksVerplicht;
	}

	public MammaLezingParameters setVerwijzenLinksVerplicht(boolean verwijzenLinksVerplicht)
	{
		this.verwijzenLinksVerplicht = verwijzenLinksVerplicht;
		return this; 
	}

	public boolean getVerbergAfrondKnop()
	{
		return verbergAfrondKnop;
	}

	public MammaLezingParameters setVerbergAfrondKnop(boolean verbergAfrondKnop)
	{
		this.verbergAfrondKnop = verbergAfrondKnop;
		return this; 
	}

	public boolean isToonOvernemenKnop()
	{
		return toonOvernemenKnop;
	}

	public MammaLezingParameters setToonOvernemenKnop(boolean toonOvernemenKnop)
	{
		this.toonOvernemenKnop = toonOvernemenKnop;
		return this;
	}

	public boolean isMetAfbeelding()
	{
		return metAfbeelding;
	}

	public MammaLezingParameters setMetAfbeelding(boolean metAfbeelding)
	{
		this.metAfbeelding = metAfbeelding;
		return this;
	}

	public MammaAmputatie getAmputatie()
	{
		return amputatie;
	}

	public MammaLezingParameters setAmputatie(MammaAmputatie amputatie)
	{
		this.amputatie = amputatie;
		return this;
	}

	public boolean isToonBiradsOpmerkingVeld()
	{
		return toonBiradsOpmerkingVeld;
	}

	public MammaLezingParameters setToonBiradsOpmerkingVeld(boolean toonBiradsOpmerkingVeld)
	{
		this.toonBiradsOpmerkingVeld = toonBiradsOpmerkingVeld;
		return this;
	}
}
