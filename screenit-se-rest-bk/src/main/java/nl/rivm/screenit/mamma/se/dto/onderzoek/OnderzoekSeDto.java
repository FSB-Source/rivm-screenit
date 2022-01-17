package nl.rivm.screenit.mamma.se.dto.onderzoek;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
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

import java.util.List;

import nl.rivm.screenit.mamma.se.dto.SeDto;
import nl.rivm.screenit.model.mamma.enums.ExtraFotosReden;
import nl.rivm.screenit.model.mamma.enums.MammaAmputatie;
import nl.rivm.screenit.model.mamma.enums.MammaOnderzoekRedenFotobespreking;
import nl.rivm.screenit.model.mamma.enums.MammaOnderzoekStatus;
import nl.rivm.screenit.model.mamma.enums.OnderbrokenOnderzoekOption;
import nl.rivm.screenit.model.mamma.enums.OnvolledigOnderzoekOption;
import nl.rivm.screenit.model.mamma.enums.SuboptimaleInsteltechniek;

public class OnderzoekSeDto extends SeDto
{

	private Integer eerderMammogramJaartal;

	private Long eerderMammogramZorginstellingId;

	private SuboptimaleInsteltechniek suboptimaleInsteltechniek;

	private MammaOnderzoekRedenFotobespreking redenFotobespreking;

	private Long extraMedewerkerId;

	private String opmerkingMbber;

	private String opmerkingVoorRadioloog;

	private boolean operatieRechts;

	private boolean operatieLinks;

	private MammaAmputatie amputatie;

	private String aanvullendeInformatieOperatie;

	private MammaOnderzoekStatus status;

	private OnvolledigOnderzoekOption onvolledigOnderzoek;

	private OnderbrokenOnderzoekOption onderbrokenOnderzoek;

	private List<ExtraFotosReden> extraFotosRedenen;

	private String adviesHuisarts;

	public Long getExtraMedewerkerId()
	{
		return extraMedewerkerId;
	}

	public void setExtraMedewerkerId(Long extraMedewerkerId)
	{
		this.extraMedewerkerId = extraMedewerkerId;
	}

	public Integer getEerderMammogramJaartal()
	{
		return eerderMammogramJaartal;
	}

	public void setEerderMammogramJaartal(Integer eerderMammogramJaartal)
	{
		this.eerderMammogramJaartal = eerderMammogramJaartal;
	}

	public Long getEerderMammogramZorginstellingId()
	{
		return eerderMammogramZorginstellingId;
	}

	public void setEerderMammogramZorginstellingId(Long eerderMammogramZorginstellingId)
	{
		this.eerderMammogramZorginstellingId = eerderMammogramZorginstellingId;
	}

	public SuboptimaleInsteltechniek getSuboptimaleInsteltechniek()
	{
		return suboptimaleInsteltechniek;
	}

	public void setSuboptimaleInsteltechniek(SuboptimaleInsteltechniek suboptimaleInsteltechniek)
	{
		this.suboptimaleInsteltechniek = suboptimaleInsteltechniek;
	}

	public MammaOnderzoekRedenFotobespreking getRedenFotobespreking()
	{
		return redenFotobespreking;
	}

	public void setRedenFotobespreking(MammaOnderzoekRedenFotobespreking redenFotobespreking)
	{
		this.redenFotobespreking = redenFotobespreking;
	}

	public String getOpmerkingMbber()
	{
		return opmerkingMbber;
	}

	public void setOpmerkingMbber(String opmerkingMbber)
	{
		this.opmerkingMbber = opmerkingMbber;
	}

	public String getOpmerkingVoorRadioloog()
	{
		return opmerkingVoorRadioloog;
	}

	public void setOpmerkingVoorRadioloog(String opmerkingVoorRadioloog)
	{
		this.opmerkingVoorRadioloog = opmerkingVoorRadioloog;
	}

	public boolean getOperatieRechts()
	{
		return operatieRechts;
	}

	public void setOperatieRechts(boolean operatieRechts)
	{
		this.operatieRechts = operatieRechts;
	}

	public boolean getOperatieLinks()
	{
		return operatieLinks;
	}

	public void setOperatieLinks(boolean operatieLinks)
	{
		this.operatieLinks = operatieLinks;
	}

	public String getAanvullendeInformatieOperatie()
	{
		return aanvullendeInformatieOperatie;
	}

	public void setAanvullendeInformatieOperatie(String aanvullendeInformatieOperatie)
	{
		this.aanvullendeInformatieOperatie = aanvullendeInformatieOperatie;
	}

	public MammaOnderzoekStatus getStatus()
	{
		return status;
	}

	public void setStatus(MammaOnderzoekStatus status)
	{
		this.status = status;
	}

	public OnvolledigOnderzoekOption getOnvolledigOnderzoek()
	{
		return onvolledigOnderzoek;
	}

	public void setOnvolledigOnderzoek(OnvolledigOnderzoekOption onvolledigOnderzoek)
	{
		this.onvolledigOnderzoek = onvolledigOnderzoek;
	}

	public OnderbrokenOnderzoekOption getOnderbrokenOnderzoek()
	{
		return onderbrokenOnderzoek;
	}

	public void setOnderbrokenOnderzoek(OnderbrokenOnderzoekOption onderbrokenOnderzoek)
	{
		this.onderbrokenOnderzoek = onderbrokenOnderzoek;
	}

	public List<ExtraFotosReden> getExtraFotosRedenen()
	{
		return extraFotosRedenen;
	}

	public void setExtraFotosRedenen(List<ExtraFotosReden> extraFotosRedenen)
	{
		this.extraFotosRedenen = extraFotosRedenen;
	}

	public String getAdviesHuisarts()
	{
		return adviesHuisarts;
	}

	public void setAdviesHuisarts(String adviesHuisarts)
	{
		this.adviesHuisarts = adviesHuisarts;
	}

	public MammaAmputatie getAmputatie()
	{
		return amputatie;
	}

	public void setAmputatie(MammaAmputatie amputatie)
	{
		this.amputatie = amputatie;
	}
}
