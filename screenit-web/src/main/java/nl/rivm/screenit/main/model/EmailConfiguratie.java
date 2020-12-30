
package nl.rivm.screenit.main.model;

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

import java.io.Serializable;

public class EmailConfiguratie implements Serializable
{

	private static final long serialVersionUID = 1L;

	private String inactiverenemail;

	private String inactiverenemailsubject;

	private String geblokkeerdemail;

	private String geblokkeerdemailsubject;

	private String registrerenhuisartsemail;

	private String registrerenhuisartsemailsubject;

	private String wachtwoordhuisartsemailsubject;

	private String wachtwoordhuisartsemail;

	public String getInactiverenemail()
	{
		return inactiverenemail;
	}

	public void setInactiverenemail(String inactiverenemail)
	{
		this.inactiverenemail = inactiverenemail;
	}

	public String getInactiverenemailsubject()
	{
		return inactiverenemailsubject;
	}

	public void setInactiverenemailsubject(String inactiverenemailsubject)
	{
		this.inactiverenemailsubject = inactiverenemailsubject;
	}

	public String getGeblokkeerdemail()
	{
		return geblokkeerdemail;
	}

	public void setGeblokkeerdemail(String geblokkeerdemail)
	{
		this.geblokkeerdemail = geblokkeerdemail;
	}

	public String getGeblokkeerdemailsubject()
	{
		return geblokkeerdemailsubject;
	}

	public void setGeblokkeerdemailsubject(String geblokkeerdemailsubject)
	{
		this.geblokkeerdemailsubject = geblokkeerdemailsubject;
	}

	public String getRegistrerenhuisartsemail()
	{
		return registrerenhuisartsemail;
	}

	public void setRegistrerenhuisartsemail(String registrerenhuisartsemail)
	{
		this.registrerenhuisartsemail = registrerenhuisartsemail;
	}

	public String getRegistrerenhuisartsemailsubject()
	{
		return registrerenhuisartsemailsubject;
	}

	public void setRegistrerenhuisartsemailsubject(String registrerenhuisartsemailsubject)
	{
		this.registrerenhuisartsemailsubject = registrerenhuisartsemailsubject;
	}

	public String getWachtwoordhuisartsemailsubject()
	{
		return wachtwoordhuisartsemailsubject;
	}

	public void setWachtwoordhuisartsemailsubject(String wachtwoordhuisartsemailsubject)
	{
		this.wachtwoordhuisartsemailsubject = wachtwoordhuisartsemailsubject;
	}

	public String getWachtwoordhuisartsemail()
	{
		return wachtwoordhuisartsemail;
	}

	public void setWachtwoordhuisartsemail(String wachtwoordhuisartsemail)
	{
		this.wachtwoordhuisartsemail = wachtwoordhuisartsemail;
	}
}
