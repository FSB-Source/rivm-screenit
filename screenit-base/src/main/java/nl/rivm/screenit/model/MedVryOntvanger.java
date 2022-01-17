package nl.rivm.screenit.model;

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

import nl.rivm.screenit.edi.model.IMedVryOntvanger;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;
import nl.rivm.screenit.model.colon.ColonHuisartsBericht;
import nl.rivm.screenit.util.NaamUtil;
import nl.topicuszorg.organisatie.model.Adres;

import org.apache.commons.lang.StringUtils;

public class MedVryOntvanger implements IMedVryOntvanger
{

	private String agbcode;

	private String volledigeOntvangerNaam;

	private String ediMailAdres;

	private String faxnummer;

	private String huisnummer;

	private String huisnummerToevoeging;

	private String plaats;

	private String postcode;

	private String straat;

	private String telefoonnummer;

	public MedVryOntvanger(ColonHuisartsBericht huisartsBericht)
	{
		if (huisartsBericht.getHuisarts() != null)
		{
			setVoorHuisarts(huisartsBericht.getHuisarts());
		}
	}

	public MedVryOntvanger(CervixHuisartsLocatie huisartsLocatie, String ediAfleverAdres)
	{
		if (huisartsLocatie != null)
		{
			setVoorHuisarts(huisartsLocatie, ediAfleverAdres);
		}
	}

	public MedVryOntvanger(Huisarts huisarts)
	{
		if (huisarts != null)
		{
			setVoorHuisarts(huisarts);
		}
	}

	private void setVoorHuisarts(Huisarts huisarts)
	{
		this.volledigeOntvangerNaam = NaamUtil.getNaamHuisarts(huisarts);
		if (StringUtils.isNotBlank(huisarts.getEdiadres()))
		{
			this.ediMailAdres = huisarts.getEdiadres();
		}
		this.agbcode = huisarts.getHuisartsAgb();
		this.telefoonnummer = huisarts.getTelefoonnummer();
		if (huisarts.getAdres() != null)
		{
			setOntvangerAdres(huisarts.getAdres());
		}
	}

	private void setOntvangerAdres(Adres adres)
	{
		this.straat = adres.getStraat();
		if (adres.getHuisnummer() != null)
		{
			this.huisnummer = adres.getHuisnummer().toString();
		}
		this.huisnummerToevoeging = adres.getHuisnummerToevoeging();
		this.plaats = adres.getPlaats();
		this.postcode = adres.getPostcode();
	}

	private void setVoorHuisarts(CervixHuisartsLocatie huisartsLocatie, String ediAfleverAdres)
	{
		CervixHuisarts huisarts = huisartsLocatie.getHuisarts();
		this.volledigeOntvangerNaam = NaamUtil.getNaamHuisarts(huisarts);

		this.ediMailAdres = ediAfleverAdres;

		this.agbcode = huisarts.getAgbcode();
		this.telefoonnummer = huisarts.getTelefoon();
		setOntvangerAdres(huisartsLocatie.getLocatieAdres());
	}

	@Override
	public String getAgbcode()
	{
		return this.agbcode;
	}

	@Override
	public String getUzinummer()
	{

		return null;
	}

	@Override
	public String getVolledigeOntvangerNaam()
	{
		return this.volledigeOntvangerNaam;
	}

	@Override
	public String getEdiMailAdres()
	{
		return this.ediMailAdres;
	}

	@Override
	public String getTelefoonnummer()
	{
		return this.telefoonnummer;
	}

	@Override
	public String getFaxnummer()
	{
		return this.faxnummer;
	}

	@Override
	public String getStraat()
	{
		return this.straat;
	}

	@Override
	public String getHuisnummer()
	{
		return this.huisnummer;
	}

	@Override
	public String getHuisnummerToevoeging()
	{
		return this.huisnummerToevoeging;
	}

	@Override
	public String getPlaats()
	{
		return this.plaats;
	}

	@Override
	public String getPostcode()
	{
		return this.postcode;
	}
}
