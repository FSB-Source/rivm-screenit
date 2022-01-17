
package nl.rivm.screenit.main.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.io.Serializable;

public class WachtwoordConfiguratie implements Serializable
{

	private static final long serialVersionUID = 1L;

	private String wachtwoordemail;

	private String wachtwoordemailsubject;

	private String uziemail;

	private String uziemailsubject;

	private Integer dagenWachtwoordGeldig;

	private Integer maximumFoutieveAanmeldpogingen;

	private Integer foutieveAanmeldpogingenTimeout;

	private Boolean wachtwoordaanvragen;

	public String getWachtwoordemail()
	{
		return wachtwoordemail;
	}

	public void setWachtwoordemail(String wachtwoordemail)
	{
		this.wachtwoordemail = wachtwoordemail;
	}

	public String getWachtwoordemailsubject()
	{
		return wachtwoordemailsubject;
	}

	public void setWachtwoordemailsubject(String wachtwoordemailsubject)
	{
		this.wachtwoordemailsubject = wachtwoordemailsubject;
	}

	public Integer getDagenWachtwoordGeldig()
	{
		return dagenWachtwoordGeldig;
	}

	public void setDagenWachtwoordGeldig(Integer dagenWachtwoordGeldig)
	{
		this.dagenWachtwoordGeldig = dagenWachtwoordGeldig;
	}

	public Integer getMaximumFoutieveAanmeldpogingen()
	{
		return maximumFoutieveAanmeldpogingen;
	}

	public void setMaximumFoutieveAanmeldpogingen(Integer maximumFoutieveAanmeldpogingen)
	{
		this.maximumFoutieveAanmeldpogingen = maximumFoutieveAanmeldpogingen;
	}

	public Integer getFoutieveAanmeldpogingenTimeout()
	{
		return foutieveAanmeldpogingenTimeout;
	}

	public void setFoutieveAanmeldpogingenTimeout(Integer foutieveAanmeldpogingenTimeout)
	{
		this.foutieveAanmeldpogingenTimeout = foutieveAanmeldpogingenTimeout;
	}

	public Boolean getWachtwoordaanvragen()
	{
		return wachtwoordaanvragen;
	}

	public void setWachtwoordaanvragen(Boolean wachtwoordaanvragen)
	{
		this.wachtwoordaanvragen = wachtwoordaanvragen;
	}

	public String getUziemail()
	{
		return uziemail;
	}

	public void setUziemail(String uziemail)
	{
		this.uziemail = uziemail;
	}

	public String getUziemailsubject()
	{
		return uziemailsubject;
	}

	public void setUziemailsubject(String uziemailsubject)
	{
		this.uziemailsubject = uziemailsubject;
	}

}
