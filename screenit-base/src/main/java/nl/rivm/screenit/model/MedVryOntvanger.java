package nl.rivm.screenit.model;

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

	public MedVryOntvanger(ColonHuisartsBericht huisartsBericht, String ossAfleverAdres)
	{
		if (huisartsBericht.getHuisarts() != null)
		{
			setVoorHuisarts(huisartsBericht.getHuisarts(), ossAfleverAdres);
		}
		else if (huisartsBericht.getOnbekendeHuisarts() != null)
		{
			setVoorOnbekendeHuisarts(huisartsBericht.getOnbekendeHuisarts(), ossAfleverAdres);
		}
	}

	public MedVryOntvanger(CervixHuisartsLocatie huisartsLocatie, String ossAfleverAdres)
	{
		if (huisartsLocatie != null)
		{
			setVoorHuisarts(huisartsLocatie, ossAfleverAdres);
		}
	}

	public MedVryOntvanger(Huisarts huisarts, String ossAfleverAdres)
	{
		if (huisarts != null)
		{
			setVoorHuisarts(huisarts, ossAfleverAdres);
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
		setVoorHuisarts(huisarts, null);
	}

	private void setVoorHuisarts(Huisarts huisarts, String ossAfleverAdres)
	{
		this.volledigeOntvangerNaam = NaamUtil.getNaamHuisarts(huisarts);
		if (StringUtils.isNotBlank(huisarts.getEdiadres()))
		{
			this.ediMailAdres = huisarts.getEdiadres();
		}
		else if (ossAfleverAdres != null)
		{
			this.ediMailAdres = ossAfleverAdres;
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

	private void setVoorHuisarts(CervixHuisartsLocatie huisartsLocatie, String ossAfleverAdres)
	{
		CervixHuisarts huisarts = huisartsLocatie.getHuisarts();
		this.volledigeOntvangerNaam = NaamUtil.getNaamHuisarts(huisarts);

		this.ediMailAdres = ossAfleverAdres;

		this.agbcode = huisarts.getAgbcode();
		this.telefoonnummer = huisarts.getTelefoon();
		setOntvangerAdres(huisartsLocatie.getLocatieAdres());
	}

	private void setVoorOnbekendeHuisarts(OnbekendeHuisarts onbekendeHuisarts, String ossAfleverAdres)
	{
		this.volledigeOntvangerNaam = NaamUtil.getNaamOnbekendeHuisarts(onbekendeHuisarts);
		this.ediMailAdres = ossAfleverAdres;
		this.telefoonnummer = onbekendeHuisarts.getTelefoonnummer();
		this.faxnummer = onbekendeHuisarts.getFaxnummer();
		this.huisnummer = new String();
		this.straat = new String();
		this.huisnummerToevoeging = new String();
		if (StringUtils.isNotBlank(onbekendeHuisarts.getPraktijkAdres()))
		{
			char[] adresArray = onbekendeHuisarts.getPraktijkAdres().toCharArray();

			for (char stukje : adresArray)
			{
				if (!Character.isDigit(stukje))
				{
					this.straat += stukje;
				}
				else
				{
					break;
				}
			}

			boolean huisnummerGevonden = false;
			for (char stukje : adresArray)
			{
				if (Character.isDigit(stukje))
				{
					this.huisnummer += stukje;
					huisnummerGevonden = true;
				}
				else if (huisnummerGevonden)
				{
					break;
				}
			}

			boolean getalGevonden = false;
			for (char stukje : adresArray)
			{
				if (Character.isDigit(stukje) && !getalGevonden)
				{
					getalGevonden = true;
				}
				else if (getalGevonden)
				{
					this.huisnummerToevoeging += stukje;
				}
			}
		}
		this.plaats = onbekendeHuisarts.getPraktijkPlaats();
		this.postcode = onbekendeHuisarts.getPraktijkPostcode();
		this.straat = StringUtils.trim(straat);
		this.huisnummer = StringUtils.trim(huisnummer);
		this.huisnummerToevoeging = StringUtils.trim(huisnummerToevoeging);
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
