package nl.rivm.screenit.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;
import java.io.StringWriter;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;

import nl.rivm.screenit.model.mamma.MammaAnnotatieAfbeelding;
import nl.rivm.screenit.model.mamma.MammaAnnotatieIcoon;
import nl.rivm.screenit.model.mamma.MammaLaesie;
import nl.rivm.screenit.model.mamma.MammaLezing;
import nl.rivm.screenit.model.mamma.enums.MammaAfbeeldingZijdeDoorsnede;
import nl.rivm.screenit.model.mamma.enums.MammaAmputatie;
import nl.rivm.screenit.model.mamma.enums.MammaAnnotatieIcoonType;
import nl.rivm.screenit.model.mamma.enums.MammaLaesieType;
import nl.rivm.screenit.model.mamma.enums.MammaZijde;
import nl.rivm.screenit.service.mamma.MammaBaseAfbeeldingService;
import nl.rivm.screenit.service.mamma.MammaBaseLaesieService;
import nl.rivm.screenit.service.mamma.MammaBaseLezingService;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import ca.uhn.hl7v2.util.StringUtil;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaBaseAfbeeldingServiceImpl implements MammaBaseAfbeeldingService
{

	private static final Logger LOG = LoggerFactory.getLogger(MammaBaseAfbeeldingServiceImpl.class);

	private static final double DOORSNEEDE_RAND_BREEDTE_IN_PIXELS = 20.0;

	private static final double VISUALELE_INSPECTIE_RAND_BREEDTE_IN_PIXELS = 100.0;

	private static final String IMAGES_MAMMA = "/images/mamma/";

	private static final String ZIJDE_DOORSNEDE_ACHTERGROND_PATH = IMAGES_MAMMA + "laesies/";

	private static final String ZIJDE_DOORSNEDE_ICONEN_PATH = IMAGES_MAMMA + "laesies/iconen/";

	private static final String INSPECTIE_ACHTERGROND_PATH = IMAGES_MAMMA + "inspectie/";

	private static final String BEOORDELING_ACHTERGROND_PATH = IMAGES_MAMMA + "beoordeling/";

	private static final String INSPECTIE_ICONEN_PATH = INSPECTIE_ACHTERGROND_PATH + "iconen/";

	@Autowired
	private MammaBaseLaesieService laesieService;

	@Autowired
	private MammaBaseLezingService lezingService;

	@Override
	public List<InputStream> createLaesiesAfbeeldingen(MammaLezing verslagLezing, MammaAmputatie amputatie)
	{
		List<InputStream> doorsneden = new ArrayList<>();

		var rechtsVerticaleDoorsnede = maakAfbeelding(MammaAfbeeldingZijdeDoorsnede.RECHTS_VERTICALE_DOORSNEDE, verslagLezing.getLaesies(), amputatie, true);
		doorsneden.add(rechtsVerticaleDoorsnede);

		var linkerborstVerticaleDoorsnede = maakAfbeelding(MammaAfbeeldingZijdeDoorsnede.LINKS_VERTICALE_DOORSNEDE, verslagLezing.getLaesies(), amputatie, true);
		doorsneden.add(linkerborstVerticaleDoorsnede);

		var rechterborstHorizontaleDoorsnede = maakAfbeelding(MammaAfbeeldingZijdeDoorsnede.RECHTS_HORIZONTALE_DOORSNEDE, verslagLezing.getLaesies(), amputatie, true);
		doorsneden.add(rechterborstHorizontaleDoorsnede);

		var linkerborstHorizontaleDoorsnede = maakAfbeelding(MammaAfbeeldingZijdeDoorsnede.LINKS_HORIZONTALE_DOORSNEDE, verslagLezing.getLaesies(), amputatie, true);
		doorsneden.add(linkerborstHorizontaleDoorsnede);

		return doorsneden;
	}

	@Override
	public InputStream createVisueleInspectieAfbeelding(MammaAnnotatieAfbeelding annotatieAfbeelding, MammaAmputatie amputatie)
	{
		final var afbeelding = getDocument(INSPECTIE_ACHTERGROND_PATH + "visuele_inspectie_afbeelding.svg");
		vulAfbeelding(afbeelding, annotatieAfbeelding, amputatie,
			VISUALELE_INSPECTIE_RAND_BREEDTE_IN_PIXELS);
		if (amputatie != null)
		{
			plaatsKruisOpVisueleInspectieAfbeelding(afbeelding, amputatie);
		}
		return maakStreamVanAfbeelding(afbeelding);
	}

	@Override
	public InputStream createSignaleringAfbeelding(MammaAnnotatieAfbeelding annotatieAfbeelding, MammaAfbeeldingZijdeDoorsnede zijdeDoorsnede, MammaAmputatie amputatie)
	{
		final var afbeelding = getZijdeDoorsnedeAfbeelding(zijdeDoorsnede, false);
		vulAfbeelding(afbeelding, annotatieAfbeelding, amputatie, 0.0);
		if (amputatie != null && lezingService.isZijdeGeamputeerd(zijdeDoorsnede, amputatie))
		{
			plaatsKruisOpSignaleringsAfbeelding(afbeelding);
		}
		return maakStreamVanAfbeelding(afbeelding);
	}

	@Override
	public InputStream createEmptyLaesiesAfbeelding(MammaAfbeeldingZijdeDoorsnede doorsnede, boolean toonLaesielocatievakken, MammaAmputatie amputatie)
	{
		var achtergrond = getDocument(BEOORDELING_ACHTERGROND_PATH + (toonLaesielocatievakken ? "metLaesielocatievakken/" : "") + doorsnede.getSvgFileName() + ".svg");
		if (amputatie != null && lezingService.isZijdeGeamputeerd(doorsnede, amputatie))
		{
			achtergrond.getDocumentElement().appendChild(createAmputatieKruis(achtergrond, false));
		}
		return maakStreamVanAfbeelding(achtergrond);
	}

	private InputStream maakAfbeelding(MammaAfbeeldingZijdeDoorsnede afbeeldingZijdeDoorsnede, List<MammaLaesie> laesies, MammaAmputatie amputatie, boolean metRand)
	{
		var achtergrond = getZijdeDoorsnedeAfbeelding(afbeeldingZijdeDoorsnede, true);
		if (amputatie != null && lezingService.isZijdeGeamputeerd(afbeeldingZijdeDoorsnede, amputatie))
		{
			voegElementToeAanAchtergrond(achtergrond, createAmputatieKruis(achtergrond, metRand));
		}
		var afbeeldingBreedte = getWidth(achtergrond);
		var factor = (afbeeldingBreedte - DOORSNEEDE_RAND_BREEDTE_IN_PIXELS * 2.0) / 100.0;
		for (var laesie : laesies)
		{
			if (isLaesieVanAfbeelding(laesie, afbeeldingZijdeDoorsnede))
			{
				var laesieAfbeelding = getLaesieAfbeelding(laesie.getMammaLaesieType());

				var heeftNummerNodig = laesieService.isVolgnummerNodig(laesies, laesie);
				var laesieSvg = bepaalDoorsnedeEnMaakLaesieElement(afbeeldingZijdeDoorsnede, laesie, laesieAfbeelding, factor, achtergrond, heeftNummerNodig);
				voegElementToeAanAchtergrond(achtergrond, laesieSvg);
			}
		}
		return maakStreamVanAfbeelding(achtergrond);
	}

	private Element createAmputatieKruis(Document achtergrond, boolean metRand)
	{
		var svgKruisContainer = achtergrond.createElement("g");
		svgKruisContainer.appendChild(createRedLine(achtergrond, 50 + (metRand ? DOORSNEEDE_RAND_BREEDTE_IN_PIXELS : 0),
			140 + (metRand ? DOORSNEEDE_RAND_BREEDTE_IN_PIXELS : 0), 125 + (metRand ? DOORSNEEDE_RAND_BREEDTE_IN_PIXELS : 0),
			230 + (metRand ? DOORSNEEDE_RAND_BREEDTE_IN_PIXELS : 0), 15));
		svgKruisContainer.appendChild(createRedLine(achtergrond, 50 + (metRand ? DOORSNEEDE_RAND_BREEDTE_IN_PIXELS : 0), 140
				+ (metRand ? DOORSNEEDE_RAND_BREEDTE_IN_PIXELS : 0), 230 + (metRand ? DOORSNEEDE_RAND_BREEDTE_IN_PIXELS : 0),
			125 + (metRand ? DOORSNEEDE_RAND_BREEDTE_IN_PIXELS : 0), 15));
		return svgKruisContainer;
	}

	private Element createRedLine(Document baseDocument, double x1, double x2, double y1, double y2, int lineWidth, String unit)
	{
		var line = baseDocument.createElement("line");
		line.setAttribute("stroke", "#623483");
		line.setAttribute("stroke-width", lineWidth + "px");
		line.setAttribute("x1", x1 + unit);
		line.setAttribute("x2", x2 + unit);
		line.setAttribute("y1", y1 + unit);
		line.setAttribute("y2", y2 + unit);
		return line;
	}

	private Element createRedLine(Document baseDocument, double x1, double x2, double y1, double y2, int lineWidth)
	{
		return createRedLine(baseDocument, x1, x2, y1, y2, lineWidth, "px");
	}

	private Document getDocument(String name)
	{
		Document document = null;
		try
		{
			var resourceAsStream = getClass().getResourceAsStream(name);
			var dbFactory = DocumentBuilderFactory.newInstance();
			var dBuilder = dbFactory.newDocumentBuilder();
			dBuilder.setEntityResolver((publicId, systemId) ->
			{
				if (systemId.contains("svg10.dtd")) 
				{
					return new InputSource(new StringReader(""));
				}
				else
				{
					return null;
				}
			});
			document = dBuilder.parse(resourceAsStream);
		}
		catch (ParserConfigurationException | SAXException | IOException | IllegalArgumentException e)
		{
			LOG.error("Er is een fout opgetreden bij het ophalen van image {}", name, e);
		}
		return document;
	}

	private boolean isLaesieVanAfbeelding(MammaLaesie laesie, MammaAfbeeldingZijdeDoorsnede afbeeldingZijdeDoorsnede)
	{
		switch (afbeeldingZijdeDoorsnede)
		{
		case RECHTS_VERTICALE_DOORSNEDE:
			return MammaZijde.RECHTER_BORST.equals(laesie.getMammaZijde()) && laesie.getVerticaleDoorsnedeIcoon() != null;
		case LINKS_VERTICALE_DOORSNEDE:
			return MammaZijde.LINKER_BORST.equals(laesie.getMammaZijde()) && laesie.getVerticaleDoorsnedeIcoon() != null;
		case RECHTS_HORIZONTALE_DOORSNEDE:
			return MammaZijde.RECHTER_BORST.equals(laesie.getMammaZijde()) && laesie.getHorizontaleDoorsnedeIcoon() != null;
		case LINKS_HORIZONTALE_DOORSNEDE:
			return MammaZijde.LINKER_BORST.equals(laesie.getMammaZijde()) && laesie.getHorizontaleDoorsnedeIcoon() != null;
		default:
			throw new IllegalStateException("Laesie is niet geldig " + laesie);
		}
	}

	private Document getLaesieAfbeelding(MammaLaesieType laesieType)
	{
		if (laesieType == null || StringUtil.isBlank(laesieType.getSvgFileName()))
		{
			throw new IllegalStateException("Geen geldige laesie type " + laesieType);
		}
		return getDocument(ZIJDE_DOORSNEDE_ICONEN_PATH + laesieType.getSvgFileName() + ".svg");
	}

	private Element bepaalDoorsnedeEnMaakLaesieElement(MammaAfbeeldingZijdeDoorsnede zijdeDoorsnede, MammaLaesie laesie, Document laesieDocument, double factor,
		Document achtergrond, boolean heeftNummerNodig)
	{
		Element laesieSvg;
		switch (zijdeDoorsnede)
		{
		case RECHTS_VERTICALE_DOORSNEDE:
		case LINKS_VERTICALE_DOORSNEDE:
			laesieSvg = maakLaesieElement(achtergrond, laesieDocument, laesie, laesie.getVerticaleDoorsnedeIcoon().getPositieX().doubleValue(),
				laesie.getVerticaleDoorsnedeIcoon().getPositieY().doubleValue(), factor, heeftNummerNodig);
			break;
		case RECHTS_HORIZONTALE_DOORSNEDE:
		case LINKS_HORIZONTALE_DOORSNEDE:
			laesieSvg = maakLaesieElement(achtergrond, laesieDocument, laesie, laesie.getHorizontaleDoorsnedeIcoon().getPositieX().doubleValue(),
				laesie.getHorizontaleDoorsnedeIcoon().getPositieY().doubleValue(), factor, heeftNummerNodig);
			break;
		default:
			throw new IllegalStateException("Laesie heeft geen doorsnede icoon " + laesie);
		}
		return laesieSvg;
	}

	private Element maakLaesieElement(Document achtergrond, Document laesieDocument, MammaLaesie laesie, double laesiePositieX, double laesiePositieY, double factor,
		boolean heeftNummerNodig)
	{
		var correctieX = getWidth(laesieDocument) / 2.0;
		var correctieY = getHeight(laesieDocument) / 2.0;

		var xLaesieHorizontaal = bepaalAfbeeldingPositie(laesiePositieX, correctieX, factor) + DOORSNEEDE_RAND_BREEDTE_IN_PIXELS;
		var yLaesieHorizontaal = bepaalAfbeeldingPositie(laesiePositieY, correctieY, factor) + DOORSNEEDE_RAND_BREEDTE_IN_PIXELS;

		var laesieSvg = maakAfbeeldingElement(laesieDocument, xLaesieHorizontaal, yLaesieHorizontaal);
		if (heeftNummerNodig)
		{
			var tekstElement = maakTekstElementEnVoegToeAanAchtergrond(achtergrond, String.valueOf(laesie.getNummer()), xLaesieHorizontaal + correctieX * 2.0,
				yLaesieHorizontaal + 10.0);
			tekstElement.setAttribute("fill", "black");
		}
		return laesieSvg;
	}

	private InputStream maakStreamVanAfbeelding(Document afbeelding)
	{
		Transformer transformer;
		try
		{
			transformer = TransformerFactory.newInstance().newTransformer();
			var writer = new StringWriter();
			transformer.transform(new DOMSource(afbeelding), new StreamResult(writer));

			var output = writer.getBuffer().toString();
			return IOUtils.toInputStream(output, StandardCharsets.UTF_8);
		}
		catch (TransformerException e)
		{
			LOG.error("Bij het transformeren naar stream van {}", afbeelding, e);
		}
		return null;
	}

	private Element maakAfbeeldingElement(Document afbeelding, double x, double y)
	{
		Element item = null;
		try
		{
			var xPath = XPathFactory.newInstance().newXPath();
			var nodeList = (NodeList) xPath.compile("/svg/g").evaluate(afbeelding, XPathConstants.NODESET);
			item = (Element) nodeList.item(0).cloneNode(true);
			item.setAttribute("transform", String.format("translate(%s,%s)", x, y));
		}
		catch (XPathExpressionException e)
		{
			LOG.error("Er is een fout opgetreden bij het ophalen van het element uit {}", afbeelding, e);
		}

		return item;
	}

	private void voegElementToeAanAchtergrond(Document achtergrond, Element item)
	{
		achtergrond.adoptNode(item);
		var rootSvgElement = getSvgNode(achtergrond);
		rootSvgElement.appendChild(item);
	}

	private double getWidth(Document afbeelding)
	{
		return Double.parseDouble(getSvgNode(afbeelding).getAttributes().getNamedItem("width").getNodeValue().replaceAll("[^0-9.]", ""));
	}

	private Node getSvgNode(Document afbeelding)
	{
		try
		{
			var xPath = XPathFactory.newInstance().newXPath();
			var nodeList = (NodeList) xPath.compile("/svg").evaluate(afbeelding, XPathConstants.NODESET);
			return nodeList.item(0);
		}
		catch (XPathExpressionException e)
		{
			LOG.error("Fout bij zoeken naar SVG deel in afbeelding", e);
		}
		return null;
	}

	private double getHeight(Document afbeelding)
	{
		return Double.parseDouble(getSvgNode(afbeelding).getAttributes().getNamedItem("height").getNodeValue().replaceAll("[^0-9.]", ""));
	}

	private double bepaalAfbeeldingPositie(double icoonPositie, double correctie, double factor)
	{
		return (icoonPositie * factor) - correctie;
	}

	private void vulAfbeelding(Document document, MammaAnnotatieAfbeelding annotatieAfbeelding, MammaAmputatie amputatie, double randBreedte)
	{
		var afbeeldingBreedte = getWidth(document);
		var factor = (afbeeldingBreedte - randBreedte * 2.0) / 100.0;
		if (annotatieAfbeelding != null)
		{
			for (var icoon : annotatieAfbeelding.getIconen())
			{
				var icoonAfbeelding = getAnnotatieIcoonAfbeelding(icoon);

				var icoonSvg = maakAnnotatieIcoonElement(icoon, icoonAfbeelding, factor, document, randBreedte);
				voegElementToeAanAchtergrond(document, icoonSvg);

			}
		}
	}

	private void plaatsKruisOpVisueleInspectieAfbeelding(Document achtergrond, MammaAmputatie amputatie)
	{
		var svgKruisContainer = achtergrond.createElement("g");
		if (MammaAmputatie.RECHTERBORST.equals(amputatie))
		{
			svgKruisContainer.appendChild(createRedLine(achtergrond, 45, 28, 75, 50, 15, "%"));
			svgKruisContainer.appendChild(createRedLine(achtergrond, 28, 45, 75, 50, 15, "%"));
		}
		else
		{
			svgKruisContainer.appendChild(createRedLine(achtergrond, 72, 55, 75, 50, 15, "%"));
			svgKruisContainer.appendChild(createRedLine(achtergrond, 55, 72, 75, 50, 15, "%"));
		}
		achtergrond.getDocumentElement().appendChild(svgKruisContainer);
	}

	private void plaatsKruisOpSignaleringsAfbeelding(Document achtergrond)
	{
		var svgKruisContainer = achtergrond.createElement("g");
		svgKruisContainer.appendChild(createRedLine(achtergrond, 25, 75, 43, 83, 15, "%"));
		svgKruisContainer.appendChild(createRedLine(achtergrond, 25, 75, 83, 43, 15, "%"));
		achtergrond.getDocumentElement().appendChild(svgKruisContainer);
	}

	private Document getAnnotatieIcoonAfbeelding(MammaAnnotatieIcoon icoon)
	{
		var annotatieIcoonType = icoon.getType();
		var svgFileName = annotatieIcoonType.getSvgFileName();
		if (StringUtils.isNotBlank(svgFileName))
		{
			return getDocument(INSPECTIE_ICONEN_PATH + svgFileName + ".svg");
		}
		else if (annotatieIcoonType.name().startsWith("SIGNALERING_"))
		{
			return getLaesieAfbeelding(MammaLaesieType.valueOf(annotatieIcoonType.name().replace("SIGNALERING_", "")));
		}
		else if (annotatieIcoonType.name().startsWith("LEGACY_"))
		{
			return getLaesieAfbeelding(MammaLaesieType.valueOf(annotatieIcoonType.name()));
		}
		throw new IllegalStateException("Ongeldige annotatie icoon type " + annotatieIcoonType);
	}

	private Document getZijdeDoorsnedeAfbeelding(MammaAfbeeldingZijdeDoorsnede zijdeDoorsnede, boolean inclusiefRand)
	{
		var svgFileName = zijdeDoorsnede.getSvgFileName();
		if (StringUtils.isNotBlank(svgFileName))
		{
			return getDocument(ZIJDE_DOORSNEDE_ACHTERGROND_PATH + (inclusiefRand ? "inclusief_rand/" : "") + svgFileName + ".svg");
		}
		throw new IllegalStateException("Ongeldige zijde doorsnede icoon type " + zijdeDoorsnede);
	}

	private Element maakAnnotatieIcoonElement(MammaAnnotatieIcoon icoon, Document icoonAfbeelding, double factor, Document achtergrond, double randBreedte)
	{
		var icoonAfbeeldingWidth = getWidth(icoonAfbeelding);
		var icoonAfbeeldingHeight = getHeight(icoonAfbeelding);
		var correctieY = icoonAfbeeldingHeight / 2.0;
		var correctieX = icoonAfbeeldingWidth / 2.0;
		if (icoon.getType() == MammaAnnotatieIcoonType.UITWENDIGE_AFWIJKING)
		{
			correctieX = icoonAfbeeldingWidth;
			correctieY = 0.0;
		}

		var xIcoon = bepaalAfbeeldingPositie(icoon.getPositieX().doubleValue(), correctieX, factor) + randBreedte;
		var yIcoon = bepaalAfbeeldingPositie(icoon.getPositieY().doubleValue(), correctieY, factor) + randBreedte;
		var element = maakAfbeeldingElement(icoonAfbeelding, xIcoon, yIcoon);
		var tekst = icoon.getTekst();
		if (StringUtils.isNotBlank(tekst))
		{
			var tekstElement = maakTekstElementEnVoegToeAanAchtergrond(achtergrond, tekst, xIcoon + icoonAfbeeldingWidth / 2.0, yIcoon + 20.0 + icoonAfbeeldingHeight);
			tekstElement.setAttribute("fill", "white");
			tekstElement.setAttribute("text-anchor", "middle");
		}
		return element;
	}

	private Element maakTekstElementEnVoegToeAanAchtergrond(Document achtergrond, String tekst, double x, double y)
	{
		var tekstElement = achtergrond.createElement("text");
		tekstElement.setAttribute("x", String.valueOf(x));
		tekstElement.setAttribute("y", String.valueOf(y));
		tekstElement.setTextContent(tekst);
		voegElementToeAanAchtergrond(achtergrond, tekstElement);
		return tekstElement;
	}

}
