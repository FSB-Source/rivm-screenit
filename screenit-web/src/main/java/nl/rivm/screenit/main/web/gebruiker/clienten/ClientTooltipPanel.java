package nl.rivm.screenit.main.web.gebruiker.clienten;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.main.service.ClientTooltipService;
import nl.rivm.screenit.model.ClientTooltip;
import nl.rivm.screenit.model.ClientTooltipType;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.OnDomReadyHeaderItem;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class ClientTooltipPanel extends GenericPanel<ClientTooltip>
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private ClientTooltipService tooltipService;

	private WebMarkupContainer ilink;

	public ClientTooltipPanel(String id, ClientTooltipType type, boolean left)
	{
		this(id, type, left, null);
	}

	public ClientTooltipPanel(String id, ClientTooltipType type, boolean left, String linkCssClass)
	{
		super(id);

		ClientTooltip tooltip = tooltipService.getClientTooltipByType(type);
		if (tooltip == null)
		{
			tooltip = new ClientTooltip();
			tooltip.setTitel(" - ");
			tooltip.setTekst(" - ");
		}

		setModel(ModelUtil.cRModel(tooltip));
		Label tekst = new Label("tekst");
		tekst.setOutputMarkupPlaceholderTag(true);
		ilink = new WebMarkupContainer("ilink");
		ilink.add(new AttributeAppender("title", Model.of(tooltip.getTitel()), " "));
		if (StringUtils.isNotBlank(linkCssClass))
		{
			ilink.add(new AttributeAppender("class", Model.of(linkCssClass), " "));
		}
		if (left)
		{
			ilink.add(new AttributeAppender("data-placement", Model.of("left"), " "));
		}
		ilink.add(new AttributeAppender("rel", Model.of(tekst.getMarkupId()), ""));
		ilink.setOutputMarkupPlaceholderTag(true);

		add(ilink);
		add(tekst);
	}

	@Override
	public void renderHead(IHeaderResponse response)
	{
		response.render(OnDomReadyHeaderItem.forScript("initInfoPopover('#" + this.ilink.getMarkupId() + "');"));
	}
}
