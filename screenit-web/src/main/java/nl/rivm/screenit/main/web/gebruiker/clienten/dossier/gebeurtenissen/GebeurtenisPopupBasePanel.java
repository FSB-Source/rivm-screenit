package nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen;

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

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.main.model.ScreeningRondeGebeurtenis;
import nl.rivm.screenit.main.model.TypeGebeurtenis;
import nl.rivm.screenit.util.EnumStringUtil;

import org.apache.commons.lang.reflect.ConstructorUtils;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;

public class GebeurtenisPopupBasePanel extends GenericPanel<ScreeningRondeGebeurtenis>
{
	private static final long serialVersionUID = 1L;

	private static final Logger LOGGER = LoggerFactory.getLogger(GebeurtenisPopupBasePanel.class);

	public GebeurtenisPopupBasePanel(String id, IModel<ScreeningRondeGebeurtenis> model)
	{
		super(id, new CompoundPropertyModel<>(model));
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		WebMarkupContainer gebeurtenisBody = new WebMarkupContainer("gebeurtenisBody");
		gebeurtenisBody.add(new AttributeAppender("class", Model.of(getModelObject().getGebeurtenis().name().toLowerCase()), " "));
		add(gebeurtenisBody);
		ScreeningRondeGebeurtenis screeningRondeGebeurtenis = getModelObject();
		add(new EnumLabel<TypeGebeurtenis>("gebeurtenis"));
		gebeurtenisBody.add(new EnumLabel<>("gebeurtenis1", screeningRondeGebeurtenis.getGebeurtenis()));

		gebeurtenisBody.add(DateLabel.forDatePattern("datum", "dd-MM-yyyy HH:mm:ss"));

		String type;
		if (screeningRondeGebeurtenis.getScreeningRondeGebeurtenissen() == null)
		{
			type = "Projecten";
			gebeurtenisBody.add(new Label("screeningRondeGebeurtenissen.rondenr", Model.of("")));
		}
		else
		{
			type = getString(EnumStringUtil.getPropertyString(screeningRondeGebeurtenis.getScreeningRondeGebeurtenissen().getScreeningRonde().getBevolkingsonderzoek()));
			gebeurtenisBody.add(new Label("screeningRondeGebeurtenissen.rondenr"));
		}

		gebeurtenisBody.add(new Label("type", type));
		getGebeurtenisDetailPanel(gebeurtenisBody, "details");
	}

	private void getGebeurtenisDetailPanel(WebMarkupContainer gebeurtenisBody, String id)
	{
		List<Object> params = new ArrayList<>();
		params.add(id);
		params.add(getModel());

		try
		{
			Class<? extends AbstractGebeurtenisDetailPanel> detailPanelClass = getModelObject().getGebeurtenis().getDetailPanelClass();
			AbstractGebeurtenisDetailPanel detailPanel = (AbstractGebeurtenisDetailPanel) ConstructorUtils.invokeConstructor(detailPanelClass, params.toArray());
			gebeurtenisBody.add(detailPanel);
			detailPanel.addButton("button", this);
			detailPanel.addExtraButton("extraButton", this);
			detailPanel.addDocumentVervangenButton("documentVervangenBtn", this);
			detailPanel.addDocumentDownloadenButton("documentDownloadBtn", this);
		}
		catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException | InstantiationException e)
		{
			LOGGER.error("Fout bij maken van gebeurtenis detailpanel", e);
			gebeurtenisBody.add(new EmptyPanel(id));
		}
	}
}
