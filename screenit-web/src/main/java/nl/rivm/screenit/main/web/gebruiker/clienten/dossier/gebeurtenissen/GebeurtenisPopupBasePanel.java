package nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.model.ScreeningRondeGebeurtenis;
import nl.rivm.screenit.main.model.TypeGebeurtenis;
import nl.rivm.screenit.util.EnumStringUtil;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

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
import org.wicketstuff.datetime.markup.html.basic.DateLabel;

@Slf4j
public class GebeurtenisPopupBasePanel extends GenericPanel<ScreeningRondeGebeurtenis>
{
	private ScreeningRondeGebeurtenis screeningRondeGebeurtenis;

	private TypeGebeurtenis typeGebeurtenis;

	public GebeurtenisPopupBasePanel(String id, IModel<ScreeningRondeGebeurtenis> model)
	{
		super(id, new CompoundPropertyModel<>(model));
		screeningRondeGebeurtenis = getModelObject();
		typeGebeurtenis = screeningRondeGebeurtenis.getGebeurtenis();
	}

	public GebeurtenisPopupBasePanel(String id, TypeGebeurtenis typeGebeurtenis)
	{
		super(id);
		this.typeGebeurtenis = typeGebeurtenis;
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		WebMarkupContainer gebeurtenisBody = new WebMarkupContainer("gebeurtenisBody");
		gebeurtenisBody.add(new AttributeAppender("class", Model.of(typeGebeurtenis.name().toLowerCase()), " "));
		add(gebeurtenisBody);
		add(new EnumLabel<TypeGebeurtenis>("gebeurtenis"));
		gebeurtenisBody.add(new EnumLabel<>("gebeurtenis1", typeGebeurtenis));

		gebeurtenisBody.add(DateLabel.forDatePattern("datum", "dd-MM-yyyy HH:mm:ss"));

		String type = "";
		if (screeningRondeGebeurtenis == null || screeningRondeGebeurtenis.getScreeningRondeGebeurtenissen() == null)
		{
			gebeurtenisBody.add(new WebMarkupContainer("screeningRondeGebeurtenissen.rondenr").setVisible(false));
		}
		else
		{
			type = getString(EnumStringUtil.getPropertyString(screeningRondeGebeurtenis.getScreeningRondeGebeurtenissen().getScreeningRonde().getBevolkingsonderzoek()));
			gebeurtenisBody.add(new Label("screeningRondeGebeurtenissen.rondenr"));
		}

		gebeurtenisBody.add(new Label("type", type));
		getGebeurtenisDetailPanel(gebeurtenisBody);
	}

	private void getGebeurtenisDetailPanel(WebMarkupContainer gebeurtenisBody)
	{
		List<Object> params = new ArrayList<>();
		params.add("details");
		params.add(getModel());

		try
		{
			Class<? extends AbstractGebeurtenisDetailPanel> detailPanelClass = typeGebeurtenis.getDetailPanelClass();
			AbstractGebeurtenisDetailPanel detailPanel = (AbstractGebeurtenisDetailPanel) ConstructorUtils.invokeConstructor(detailPanelClass, params.toArray());
			gebeurtenisBody.add(detailPanel);
			detailPanel.addButton("button", this);
			detailPanel.addExtraButton("extraButton", this);
			detailPanel.addDocumentVervangenButton("documentVervangenBtn", this);
			detailPanel.addDocumentDownloadenButton("documentDownloadBtn", this);
		}
		catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException | InstantiationException e)
		{

			LOG.error("Fout bij maken van gebeurtenis detailpanel", e);
			gebeurtenisBody.add(new EmptyPanel("details"));
		}
	}

	@Override
	protected void detachModel()
	{
		super.detachModel();
		ModelUtil.nullSafeDetach(screeningRondeGebeurtenis);
	}
}
