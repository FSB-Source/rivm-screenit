package nl.rivm.screenit.main.web.client.dashboard;

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

import java.text.SimpleDateFormat;
import java.util.Date;

import nl.rivm.screenit.main.web.client.base.ClientHoofdMenuitem;
import nl.rivm.screenit.main.web.gebruiker.clienten.ClientTooltipPanel;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientTooltipType;
import nl.rivm.screenit.model.TijdelijkAdres;
import nl.rivm.screenit.util.AdresUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.NaamUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.commons.lang3.StringUtils;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.link.Link;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class ClientGegevensPanel extends GenericPanel<Client>
{
	@SpringBean
	private HibernateService hibernateService;

	private ClientHoofdMenuitem menu;

	public ClientGegevensPanel(String id, IModel<String> title, final IModel<Client> clientModel, ClientHoofdMenuitem menu)
	{
		super(id, new CompoundPropertyModel<>(clientModel));
		this.menu = menu;

		persoonsgegevensBlock();
		tijdelijkAdresBlock();
	}

	private void persoonsgegevensBlock()
	{
		add(new Label("naam", NaamUtil.titelVoorlettersTussenvoegselEnAanspreekAchternaam(getModelObject())));
		add(new ClientTooltipPanel("gbaAdresTooltip", ClientTooltipType.GBA_ADRES, false, "pull-right"));
		add(new Label("persoon.geboortedatum", DateUtil.getGeboortedatum(getModelObject())));
		add(new Label("persoon.adres", new IModel<String>()
		{
			private static final long serialVersionUID = 1L;

			@Override
			public String getObject()
			{
				return org.apache.wicket.util.string.Strings.escapeMarkup(AdresUtil.getVolledigeGbaAdresString(getModelObject().getPersoon())).toString().replaceAll(",", "<br>");
			}
		}).setEscapeModelStrings(false));
		add(new Label("persoon.bsn"));
	}

	private void tijdelijkAdresBlock()
	{
		Label tijdelijkAdresContainer = new Label("persoon.adres.tijdelijk", new IModel<String>()
		{
			private static final long serialVersionUID = 1L;

			@Override
			public String getObject()
			{
				TijdelijkAdres adres = getModelObject().getPersoon().getTijdelijkAdres();
				String adresString = "";
				if (adres != null)
				{
					adresString = org.apache.wicket.util.string.Strings.escapeMarkup(AdresUtil.getVolledigeAdresString(adres)).toString().replaceAll(",", "<br>");
					SimpleDateFormat format = new SimpleDateFormat("dd-MM-yyyy");
					Date start = adres.getStartDatum();
					if (start != null)
					{
						adresString += "<br> Van " + format.format(start);
					}
					Date eind = adres.getEindDatum();
					if (eind != null)
					{
						if (adresString.contains("<br> Van "))
						{
							adresString += " tot ";
						}
						else
						{
							adresString += "<br> Tot ";
						}
						adresString += format.format(eind);
					}

				}
				return adresString;
			}
		});
		add(tijdelijkAdresContainer);
		tijdelijkAdresContainer.setEscapeModelStrings(false);

		Link<Client> wijzigTijdelijkadres = new Link<Client>("wijzigTijdelijkadres", getModel())
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick()
			{
				setResponsePage(new ClientWijzigTijdelijkadresPage(ModelUtil.ccModel(ClientGegevensPanel.this.getModelObject()), menu));
			}

		};
		add(wijzigTijdelijkadres);
		TijdelijkAdres tijdelijkAdres = getModelObject().getPersoon().getTijdelijkAdres();
		boolean hasTijdelijkAdres = tijdelijkAdres != null
			&& (tijdelijkAdres.getHuisnummer() != null || StringUtils.isNotBlank(tijdelijkAdres.getHuisnummerToevoeging()) || StringUtils.isNotBlank(tijdelijkAdres.getPlaats())
				|| StringUtils.isNotBlank(tijdelijkAdres.getPostcode()) || StringUtils.isNotBlank(tijdelijkAdres.getStraat()));
		String buttonLabel = "label.tijdelijkadres";
		if (hasTijdelijkAdres)
		{
			buttonLabel += "wijzigen";
		}
		else
		{
			buttonLabel += "opgeven";
		}
		wijzigTijdelijkadres.add(new Label("buttonLabel", getString(buttonLabel)));

		tijdelijkAdresContainer.setVisible(hasTijdelijkAdres);

		add(new ClientTooltipPanel("tijdelijkAdresTooltip", ClientTooltipType.TIJDELIJK_ADRES, false, "pull-right"));
	}
}
