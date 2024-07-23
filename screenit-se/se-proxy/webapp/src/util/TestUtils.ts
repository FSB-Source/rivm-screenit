import renderer, {ReactTestRenderer} from "react-test-renderer"

export function rerender(component: ReactTestRenderer): renderer.ReactTestRendererJSON {
	return component.toJSON() as renderer.ReactTestRendererJSON
}
