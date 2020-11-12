package nl.rivm.screenit.service.mamma.impl;

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

import nl.rivm.screenit.model.mamma.MammaLaesie;
import nl.rivm.screenit.model.mamma.MammaLaesieIcoon;
import nl.rivm.screenit.model.mamma.enums.MammaZijde;
import nl.rivm.screenit.service.mamma.MammaLaesieLocatieService;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaLaesieLocatieServiceImpl implements MammaLaesieLocatieService
{

	private final int verticaleDoorsnedeBovenkantTepelvak = 81;

	private final int verticaleDoorsnedeOnderkantTepelvak = 98;

	private final int verticaleDoorsnedeHorizontaleLijn = 90;

	private final int verticaleDoorsnedeVoorsteEenderde = 65; 

	private final int verticaleDoorsnedeAchtersteEenderde = 31; 

	private final int horizontaleDoorsnedeBovenkantTepelvak = 92;

	private final int horizontaleDoorsnedeOnderkantTepelvak = 110;

	private final int horizontaleDoorsnedeHorizontaleLijn = 101;

	private final int horizontaleDoorsnedeVoorsteEenderde = 62; 

	private final int horizontaleDoorsnedeAchtersteEenderde = 31; 

	public String laesie2diepte(MammaLaesie laesie)
	{
		MammaLaesieIcoon horizontaleDoorsnedeIcoon = laesie.getHorizontaleDoorsnedeIcoon();
		if (horizontaleDoorsnedeIcoon != null)
		{
			return icoon2diepte(laesie, horizontaleDoorsnedeIcoon.getPositieX().intValue(), horizontaleDoorsnedeVoorsteEenderde, horizontaleDoorsnedeAchtersteEenderde);
		}
		else
		{ 
			return icoon2diepte(laesie, laesie.getVerticaleDoorsnedeIcoon().getPositieX().intValue(), verticaleDoorsnedeVoorsteEenderde, verticaleDoorsnedeAchtersteEenderde);
		}
	}

	private String icoon2diepte(MammaLaesie laesie, int x, int voorsteEenderde, int achtersteEenderde)
	{
		if (laesie.getMammaZijde() == MammaZijde.RECHTER_BORST)
		{
			if (x < 100 - voorsteEenderde)
			{
				return "voorste";
			}
			else if (x > 100 - achtersteEenderde)
			{
				return "achterste";
			}
		}
		else
		{ 
			if (x > voorsteEenderde)
			{
				return "voorste";
			}
			else if (x < achtersteEenderde)
			{
				return "achterste";
			}
		}
		return "middelste";
	}

	public String laesie2kwadrant(MammaLaesie laesie)
	{
		MammaLaesieIcoon verticaleDoorsnedeIcoon = laesie.getVerticaleDoorsnedeIcoon();
		MammaLaesieIcoon horizontaleDoorsnedeIcoon = laesie.getHorizontaleDoorsnedeIcoon();
		if (isRetroMamillair(laesie, verticaleDoorsnedeIcoon, horizontaleDoorsnedeIcoon))
		{
			return "retromamillair";
		}
		String result = "kwadrant";
		if (verticaleDoorsnedeIcoon != null)
		{
			result = (verticaleDoorsnedeIcoon.getPositieY().intValue() < verticaleDoorsnedeHorizontaleLijn ? "boven" : "onder") + result;
		}
		if (horizontaleDoorsnedeIcoon != null)
		{
			result = (horizontaleDoorsnedeIcoon.getPositieY().intValue() < horizontaleDoorsnedeHorizontaleLijn ? "laterale " : "mediale ") + result;
		}
		return result;
	}

	private boolean isRetroMamillair(MammaLaesie laesie, MammaLaesieIcoon verticaleDoorsnedeIcoon, MammaLaesieIcoon horizontaleDoorsnedeIcoon)
	{
		return (verticaleDoorsnedeIcoon == null || inTepelvakVerticaleDoorsnede(laesie, verticaleDoorsnedeIcoon)) &&
			(horizontaleDoorsnedeIcoon == null || inTepelvakHorizontaleDoorsnede(laesie, horizontaleDoorsnedeIcoon));
	}

	private boolean inTepelvakVerticaleDoorsnede(MammaLaesie laesie, MammaLaesieIcoon verticaleDoorsnedeIcoon)
	{
		int x = verticaleDoorsnedeIcoon.getPositieX().intValue();
		int y = verticaleDoorsnedeIcoon.getPositieY().intValue();
		if (laesie.getMammaZijde() == MammaZijde.RECHTER_BORST)
		{
			return x < (100 - verticaleDoorsnedeVoorsteEenderde) && y > verticaleDoorsnedeBovenkantTepelvak && y < verticaleDoorsnedeOnderkantTepelvak;
		}
		else
		{ 
			return x > verticaleDoorsnedeVoorsteEenderde && y > verticaleDoorsnedeBovenkantTepelvak && y < verticaleDoorsnedeOnderkantTepelvak;
		}
	}

	private boolean inTepelvakHorizontaleDoorsnede(MammaLaesie laesie, MammaLaesieIcoon horizontaleDoorsnedeIcoon)
	{
		int x = horizontaleDoorsnedeIcoon.getPositieX().intValue();
		int y = horizontaleDoorsnedeIcoon.getPositieY().intValue();
		if (laesie.getMammaZijde() == MammaZijde.RECHTER_BORST)
		{
			return x < (100 - horizontaleDoorsnedeVoorsteEenderde) && y > horizontaleDoorsnedeBovenkantTepelvak && y < horizontaleDoorsnedeOnderkantTepelvak;
		}
		else
		{ 
			return x > horizontaleDoorsnedeVoorsteEenderde && y > horizontaleDoorsnedeBovenkantTepelvak && y < horizontaleDoorsnedeOnderkantTepelvak;
		}
	}
}
