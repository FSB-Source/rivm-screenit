package nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.afbeelding;

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

import java.io.InputStream;
import java.util.List;
import java.util.function.Consumer;

import nl.rivm.screenit.main.web.component.SvgImage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.dto.LaesieDto;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.dto.LaesieDtoMapper;
import nl.rivm.screenit.model.mamma.enums.MammaAfbeeldingZijdeDoorsnede;
import nl.rivm.screenit.model.mamma.enums.MammaAmputatie;
import nl.rivm.screenit.service.mamma.MammaBaseAfbeeldingService;
import nl.rivm.screenit.service.mamma.MammaBaseLezingService;

import org.apache.wicket.ajax.AbstractDefaultAjaxBehavior;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.attributes.AjaxRequestAttributes;
import org.apache.wicket.ajax.attributes.CallbackParameter;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.JavaScriptHeaderItem;
import org.apache.wicket.markup.head.OnDomReadyHeaderItem;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.request.resource.IResource;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MammaLaesiesAfbeeldingPanel extends MammaBaseAfbeeldingPanel<List<LaesieDto>>
{

	public static final int UNSAVED_DEFAULT_LEZING_ID = 0;

	private static final String JSON_PARAM = "jsonparam";

	private final boolean alleenInzien;

	private final Long lezingId;

	private final MammaAmputatie amputatie;

	private LaesieDtoMapper laesieDtoMapper = new LaesieDtoMapper();

	private final AbstractDefaultAjaxBehavior updateLaesiesBehavior;

	private Consumer<AjaxRequestTarget> onAfbeeldingGewijzigd;

	private boolean toonLaesielocatievakken;

	@SpringBean
	private MammaBaseAfbeeldingService baseAfbeeldingService;

	@SpringBean
	private MammaBaseLezingService lezingService;

	public MammaLaesiesAfbeeldingPanel(String id, IModel<List<LaesieDto>> model, boolean alleenInzien, Long lezingId, MammaAmputatie amputatie)
	{
		this(id, model, alleenInzien, lezingId, false, amputatie);
	}

	public MammaLaesiesAfbeeldingPanel(String id, IModel<List<LaesieDto>> model, boolean alleenInzien, Long lezingId, boolean toonLaesielocatievakken, MammaAmputatie amputatie)
	{
		super(id, model);
		this.alleenInzien = alleenInzien;
		this.lezingId = lezingId != null ? lezingId : UNSAVED_DEFAULT_LEZING_ID;
		this.toonLaesielocatievakken = toonLaesielocatievakken;
		this.amputatie = amputatie;
		updateLaesiesBehavior = createUpdateLaesiesBehavior();
		add(updateLaesiesBehavior);
		add(new AttributeAppender("x-lezing-id", Model.of(String.valueOf(this.lezingId))));
		add(new AttributeAppender("class", Model.of("afbeeldingPanel"), " "));
	}

	@Override
	protected void addImage(WebMarkupContainer imageContainer, MammaAfbeeldingZijdeDoorsnede doorsnede)
	{
		imageContainer.add(new AttributeAppender("ondragover", Model.of("dragover(event)")));
		SvgImage img = new SvgImage(doorsnede.getSvgFileName())
		{
			@Override
			protected InputStream getSvgImageData(IResource.Attributes attributes)
			{
				return baseAfbeeldingService.createEmptyLaesiesAfbeelding(doorsnede, toonLaesielocatievakken, amputatie);
			}
		};
		imageContainer.add(img);
		if (lezingService.isZijdeGeamputeerd(doorsnede, amputatie))
		{
			imageContainer.add(new AttributeAppender("x-amputatie", "true"));
		}
		else
		{
			imageContainer.add(new AttributeAppender("x-amputatie", "false"));
		}
	}

	@Override
	protected Panel createBeoordelingIconenPanel(String id)
	{
		return alleenInzien ? super.createBeoordelingIconenPanel(id) : new MammaBeoordelingIconenPanel(id);
	}

	private AbstractDefaultAjaxBehavior createUpdateLaesiesBehavior()
	{
		return new AbstractDefaultAjaxBehavior()
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected void updateAjaxAttributes(AjaxRequestAttributes attributes)
			{
				super.updateAjaxAttributes(attributes);
				attributes.setMethod(AjaxRequestAttributes.Method.POST);
			}

			@Override
			protected void respond(AjaxRequestTarget target)
			{
				String json = getComponent().getRequest().getRequestParameters().getParameterValue(JSON_PARAM).toString();
				MammaLaesiesAfbeeldingPanel.this.setModelObject(laesieDtoMapper.laesieJsonToLaesieDto(json));

				if (onAfbeeldingGewijzigd != null)
				{
					onAfbeeldingGewijzigd.accept(target);
				}
			}
		};
	}

	@Override
	public void renderHead(IHeaderResponse response)
	{
		super.renderHead(response);
		response.render(JavaScriptHeaderItem.forUrl("assets/js/laesieiconen/data.js"));
		response.render(JavaScriptHeaderItem.forUrl("assets/js/laesieiconen/mouseevents.js"));
		response.render(JavaScriptHeaderItem.forUrl("assets/js/laesieiconen/opslaan.js"));
		response.render(JavaScriptHeaderItem.forUrl("assets/js/laesieiconen/vulafbeelding.js"));
		response.render(OnDomReadyHeaderItem.forScript(getToonLaesiesFunctie()));
	}

	private String getToonLaesiesFunctie()
	{
		String laesiesJson = laesieDtoMapper.laesiesDtosToJson(getModelObject());
		return "toonLaesies('" + laesiesJson + "'" + "," + alleenInzien + ",'" + lezingId + "'," +
			updateLaesiesBehavior.getCallbackFunction(CallbackParameter.explicit(JSON_PARAM)) + ");";
	}

	public void setOnAfbeeldingGewijzigd(Consumer<AjaxRequestTarget> onAfbeeldingGewijzigd)
	{
		this.onAfbeeldingGewijzigd = onAfbeeldingGewijzigd;
	}
}
